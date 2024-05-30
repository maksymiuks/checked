new_check_design <- function(...) {
  check_design$new(...)
}

new_rev_dep_check_design <- function(x, ...) {
  tasks <- rev_dep_check_tasks_df(x)
  new_check_design(tasks, ...)
}

#' @examples
#' df <- rev_dep_check_tasks_df("~/Desktop/validation/code/DALEX/")
#' plan <- check_design$new(df, n = 20)
#' while (!plan$is_done()) {
#'   print(table(igraph::vertex.attributes(plan$graph)$status |> as.character()))
#'   plan$start_next_task()
#' }
#'
check_design <- R6::R6Class(
  "check_design",
  public = list(
    #' A dependency graph, storing information about which dependencies
    #' are required prior to execution of each check task.
    graph = NULL,

    #' source of check design
    input = NULL,

    #' output root directory
    output = tempfile(paste(packageName(), Sys.Date(), sep = "-")),

    #' Initialize a new check design
    #' repos passed here will be used only fetch dependencies. Source of
    #' packages to be check are embedded in the df and might very well be
    #' different repos.
    initialize = function(
        # styler: off
        df,
        n = 2L,
        output = tempfile(paste(packageName(), Sys.Date(), sep = "-")),
        lib.loc = .libPaths(),
        repos = getOption("repos"),
        ...) { # styler: on
      self$input <- df
      self$output <- output
      private$n <- n
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
      invisible(lapply(private$active, function(process) process$finalize()))
    },

    #' Fill Available Processes with Tasks
    #' @return A logical value, indicating whether processes are actively
    #'   running.
    step = function() {
      while ((res <- self$start_next_task()) > 0) {}
      res >= 0
    },

    #' Start Next Task
    #' @return A integer value, coercible to logical to indicate whether a new
    #'   process was spawned, or `-1` if all tasks have finished.
    start_next_task = function() {
      # finalize any finished processes
      for (process in private$active) {
        if (!process$is_alive()) {
          process$finalize()
        }
      }

      # if all available processes are in use, terminate early
      n_active <- length(private$active)
      if (n_active >= private$n) {
        return(0L)
      }

      next_task <- next_task_to_run(self$graph, self$output)
      if (length(next_task) > 0) {
        process <- start_task(
          task = next_task,
          g = self$graph,
          output = self$output,
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
      task_graph_task_process(self$graph, task) <- x
      name <- task_graph_task_name(self$graph, task)
      task_graph_package_status(self$graph, task) <- STATUS$`in progress`
      x$set_finalizer(function(process) {
        self$pop_process(name)
        task_graph_package_status(self$graph, task) <- STATUS$done
      })
      private$active[[name]] <- x
      TRUE
    },
    is_done = function() {
      all(igraph::vertex.attributes(self$graph)$status == STATUS$done)
    }
  ),
  private = list(
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
  print(x$input)
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

#' Attempt to Recover Previous Run State
#'
#'
# TODO: reimplement restoration when API is done

# rev_dep_attempt_restore <- function(
#     path,
#     restore = if (interactive()) NA else TRUE) {
#   if (missing(path) || !file.exists(path)) {
#     return(character(0L))
#   }
#
#   restore <- if (interactive() && is.na(restore)) {
#     startsWith(toupper(trimws(readline(paste0(
#       "Output path '", path, "' already exists. Do you want ",
#       "to attempt to recover a previous run? [Y/n]"
#     )))), "Y")
#   }
#
#   if (isFALSE(restore)) {
#     unlink(path, recursive = TRUE)
#     return(character(0L))
#   } else {
#     check_dirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
#     names(check_dirs) <- basename(check_dirs)
#     statuses <- vcapply(check_dirs, function(path) {
#       STATUSES[STATUSES %in% list.files(path, include.dirs = FALSE)]
#     })
#     return(statuses)
#   }
# }
