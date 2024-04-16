new_check_design <- function(...) {
  check_design$new(...)
}

new_rev_dep_check_design <- function(x, ...) {
  tasks <- rev_dep_check_tasks_df(x)
  new_check_design(tasks, ...)
}

#' @examples
#' package_source_dir <- "../praise"
#' x <- new_rev_dep_check_design(package_source_dir, n = 10L)
#' while (any(x$checks$process != "done")) {
#'   while (x$step()) {}
#'   cat("Processes: \n")
#'   cat(paste0(" * ", names(x$.__enclos_env__$private$active), collapse = "\n"), "\n")
#'   print(x)
#'   Sys.sleep(1)
#' }
check_design <- R6::R6Class(
  "check_design",
  public = list(
    #' A dependency graph, storing information about which dependencies
    #' are required prior to execution of each check task.
    graph = NULL,

    #' Specifies the tasks that need to be performed. When preparing to execute
    #' these tasks, other tasks may need to be inserted, such as dependency
    #' installation tasks needed before a check task can be run.
    checks = data.frame(
      alias = character(0L), # a display name for the task
      type = character(0L), # a task type
      package = character(0L), # package name
      version = package_version(NA, strict = FALSE)[c()], # package version
      cmd = list(), # command to issue in a separate process
      process = list() # process enum, or processx process object
    ),

    #' Initialize a new check design
    initialize = function(x, n = 2L, ...) {
      private$n <- n
      checks <- new_check_tasks_df(x, ...)
      self$graph <- dep_graph_create(checks$package)
      idx <- order(match(checks$package, igraph::V(self$graph)$name))
      checks <- checks[order(idx), ]
      rownames(checks) <- NULL
      self$checks <- checks
    },

    #' Step Through Tasks
    #' @return A logical value indicating whether a new process was spawned
    step = function() {
      # finalize any finished processes
      for (process in private$active) {
        if (!process$is_alive()) process$finalize()
      }

      # if all available processes are in use, terminate early
      n_active <- length(private$active)
      if (n_active >= private$n) {
        return(FALSE)
      }

      # if a check package's dependencies are satisfied, proceed with checks
      ready_roots <- dep_graph_which_root_satisfied(self$graph)
      ready_indices <- with(self$checks, {
        which(package %in% ready_roots & process == "todo")
      })
      if (length(ready_indices) > 0) {
        idx <- head(ready_indices, 1L)
        alias <- self$checks[[idx, "alias"]]

        process <- mock_process$new(runif(1, 15, 25)) # fake process, lasts ~20s
        # this process needs to:
        #   - acquire source code of package to check
        #   - set up appropriate, restricted library, linking to cache
        #   - run r cmd check

        success <- self$push_check_process(idx, process)
        return(success)
      }

      # if we can't run a new package checks, install next dependencies
      next_dep_install <- dep_graph_which_satisfied_strong(self$graph)
      if (length(next_dep_install) > 0) {
        package <- head(next_dep_install, 1L)

        process <- mock_process$new(runif(1, 1, 3)) # fake process, lasts ~2s
        # this process needs to:
        #   - install a package into the shared cache library

        success <- self$push_install_process(package, process)
        return(success)
      }

      FALSE
    },
    push_check_process = function(idx, x) {
      alias <- self$checks[[idx, "alias"]]
      self$checks[[idx, "process"]] <- "in progress"
      self$push_process(alias, x)
      x$set_finalizer(function(process) {
        self$pop_process(alias)
        self$checks[[idx, "process"]] <- "done"
      })
      TRUE
    },
    push_install_process = function(package, x) {
      alias <- paste("install", "dep", package, sep = "-")
      graph_idx <- which(igraph::V(self$graph)$name == package)
      igraph::vertex_attr(self$graph, "status", graph_idx) <- "in progress"
      x$set_finalizer(function(process) {
        self$pop_process(alias)
        igraph::vertex_attr(self$graph, "status", graph_idx) <- "done"
      })
      self$push_process(alias, x)
      TRUE
    },
    pop_process = function(name) {
      private$active[[name]] <- NULL
    },
    push_process = function(name, x) {
      private$active[[name]] <- x
    }
  ),
  private = list(
    # output root directory
    output = tempfile(paste(packageName(), Sys.Date(), sep = "-")),
    # maximum child process count
    n = 2L,
    # active processes
    active = list()
  )
)



new_check_tasks_df <- function(x) {
  req_cols <- c("alias", "type", "package", "version", "cmd")

  checkmate::assert_data_frame(x)
  checkmate::assert_names(names(x), must.include = req_cols)

  # clean up data frame structure
  x <- x[c(req_cols, setdiff(colnames(x), req_cols))]
  rownames(x) <- NULL

  # add process indicator
  if (is.null(x$process)) {
    x$process <- list("todo")
  } else {
    x$process[which(is.na(x$process))] <- list("todo")
  }

  # reorder columns for consistency
  x <- x[, c(req_cols, setdiff(colnames(x), req_cols))]

  x
}

#' @export
print.check_design <- function(x, ...) {
  print(x$checks)
}

#' @export
as_check_tasks_df <- function(x) {
  UseMethod("as_check_tasks_df")
}

#' @export
as_check_tasks_df.character <- function(x, ...) {
  package <- x
  version <- available.packages()[package, "Version"]
  args <- list(...)
  alias <- package

  df <- data.frame(
    alias = alias,
    type = "check",
    package = package,
    version = version
  )

  df$cmd <- rep_len(list(args), length(package))
  new_check_tasks_df(df)
}

#' Build Plan for Checking Reverse Dependencies from Package Source
#'
#' @param path A file path to a local package source code directory of the
#'   package whose reverse dependencies are to be checked.
#' @param output A file path to a directory in which logs and other check
#'   metadata should be saved. Defaults to a new temporary directory.
#' @param ... Additional arguments passed to individual
#'   [`rcmdcheck::rcmdcheck`] calls
#' @param restore Whether existing logs in `output` should be used to try to
#'   restore a previous run. When in an interactive session, `NA` will indicate
#'   that you should be prompted to confirm. Otherwise, a logical value
#'   indicates whether to restore from or overwrite the existing logs.
#'
#' @return A check plan
#'
#' @examples
#' \dontrun{
#' pkg_source_dir <- "praise"
#' rev_dep_check_plan(pkg_source_dir)
#' }
#'
#' @importFrom tools package_dependencies
#' @export
rev_dep_check_tasks_df <- function(
    path,
    output = tempfile(paste(packageName(), Sys.Date(), sep = "-")),
    ...,
    restore) {
  path <- check_path_is_pkg_source(path)
  package <- get_package_name(path)
  package_v <- available.packages()[package, "Version"]
  revdeps <- tools::package_dependencies(package, reverse = TRUE)[[1]]
  restored_statuses <- rev_dep_attempt_restore(output, restore)

  # build reverse dependencies data frame
  df_dev <- df_rel <- as_check_tasks_df(revdeps, ...)
  df_dev$alias <- paste0(df_dev$alias, " (+", package, "_dev)")
  df_rel$alias <- paste0(df_rel$alias, " (+", package, "_v", package_v, ")")
  idx <- rep(seq_len(nrow(df_rel)), each = 2) + c(0, nrow(df_rel))
  df <- rbind(df_dev, df_rel)[idx, ]
  df$process <- restored_statuses[match(df$alias, names(restored_statuses))]

  # re-structure data frame as a plan object
  new_check_tasks_df(df)
}

#' Attempt to Recover Previous Run State
#'
rev_dep_attempt_restore <- function(
    path,
    restore = if (interactive()) NA else TRUE) {
  if (missing(path) || !file.exists(path)) {
    return(character(0L))
  }

  restore <- if (interactive() && is.na(restore)) {
    startsWith(toupper(trimws(readline(paste0(
      "Output path '", path, "' already exists. Do you want ",
      "to attempt to recover a previous run? [Y/n]"
    )))), "Y")
  }

  if (isFALSE(restore)) {
    unlink(path, recursive = TRUE)
    return(character(0L))
  } else {
    check_dirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
    names(check_dirs) <- basename(check_dirs)
    statuses <- vcapply(check_dirs, function(path) {
      STATUSES[STATUSES %in% list.files(path, include.dirs = FALSE)]
    })
    return(statuses)
  }
}
