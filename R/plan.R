new_check_design <- function(...) {
  check_design$new(...)
}

new_rev_dep_check_design <- function(x, ...) {
  tasks <- rev_dep_check_tasks_df(x)
  new_check_design(tasks, ...)
}

#' @examples
#' package_source_dir <- "../praise"
#' df <- rev_dep_check_tasks_df("~/Desktop/validation/code/DALEX/")
#' plan <- check_design$new(df, n = 20)
#' while (!all(as.character(igraph::vertex.attributes(plan$graph)$status) == "done")) {
#'   print(table(igraph::vertex.attributes(plan$graph)$status |> as.character()))
#'   plan$start_next_task()
#' }
#' }
check_design <- R6::R6Class(
  "check_design",
  public = list(
    #' A dependency graph, storing information about which dependencies
    #' are required prior to execution of each check task.
    graph = NULL,

    #' Initialize a new check design
    #' repos passed here will be used only fetch dependencies. Source of packages
    #' to be check are embedded in the df and might very well be different repos.
    initialize = function(
      df, n = 2L, output = tempfile(paste(packageName(), Sys.Date(), sep = "-")), 
      lib.loc = .libPaths(), repos = getOption("repos"), ...) {
      private$n <- n
      private$output <- output
      private$lib.loc <- lib.loc
      private$repos <- repos
      
      g <- task_graph_create(df, repos)
      self$graph <- task_graph_update_done(g, lib.loc)
      
    },

    #' Get Active Processes
    active_processes = function() {
      private$active
    },

    #' Terminate Design Processes
    terminate = function() {
      invisible(lapply(private$active, function(process) process$kill()))
    },

    #' Fill Available Processes with Tasks
    #' @return A logical value, indicating whether processes are actively
    #'   running.
    step = function() {
      while (res <- self$start_next_task()) {}
      res >= 0
    },

    #' Start Next Task
    #' @return A integer value, coercible to logical to indicate whether a new
    #'   process was spawned, or `-1` if all tasks have finished.
    start_next_task = function() {
      # finalize any finished processes
      for (process in private$active) {
        if (!process$is_alive()) process$finalize()
      }

      # if all available processes are in use, terminate early
      n_active <- length(private$active)
      if (n_active >= private$n) {
        return(0L)
      }

      next_task <- next_task_to_run(self$graph, private$output)
      if (length(next_task) > 0) {
        process <- start_task(
          task = next_task, 
          output = private$output, 
          lib.loc = private$lib.loc
        )
        success <- self$push_process(next_task, process)
        return(as.integer(success))
      }

      finished <- (length(private$active) == 0) && self$is_done()
      return(-finished)
    },
    get_process = function(name) {
      private$active[[name]]
    },
    pop_process = function(name) {
      private$active[[name]] <- NULL
    },
    push_process = function(task, x) {
      name <- names(task$v)
      task_graph_package_status(self$graph, task$v) <- STATUS$`in progress`
      x$set_finalizer(function(process) {
        self$pop_process(name)
        task_graph_package_status(self$graph, task$v) <- STATUS$done
      })
      private$active[[name]] <- x
      TRUE
    },
    is_done = function() {
      all(igraph::vertex.attributes(self$graph)$status == STATUS$done)
    }
  ),
  private = list(
    # output root directory
    output = tempfile(paste(packageName(), Sys.Date(), sep = "-")),
    # maximum child process count
    n = 2L,
    # lib.loc of allowed packages,
    lib.loc = .libPaths(),
    # repositories to fetch dependencies from
    repos = getOption("repos"),
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
    x$process <- list(STATUS$pending)
  } else {
    x$process[which(is.na(x$process))] <- list(STATUS$pending)
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
  df_dev$alias <- paste0(df_dev$alias, " (dev)")
  df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
  idx <- rep(seq_len(nrow(df_rel)), each = 2) + c(0, nrow(df_rel))
  df <- rbind(df_dev, df_rel)[idx, ]

  # restore process statuses recovered from previous output
  idx <- which(df$alias %in% names(restored_statuses))
  df$process[idx] <- STATUS[restored_statuses[df$alias[idx]]]

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
