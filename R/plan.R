new_check_design <- function(...) {
  check_design$new(...)
}

new_rev_dep_check_design <- function(x, ...) {
  tasks <- rev_dep_check_tasks_df(x)
  new_check_design(tasks, ...)
}

check_design <- R6::R6Class(
  "check_design",
  public = list(
    # <NULL,igraph>
    graph = NULL,
    # <data.frame>
    tasks = data.frame(
      alias = character(0L), # a display name for the task
      type = character(0L), # a task type
      package = character(0L), # package name
      version = package_version(NA, strict = FALSE)[c()], # package version
      cmd = list(), # command to issue in a separate process
      process = list() # process enum, or processx process object
    ),
    initialize = function(x, ...) {
      tasks <- new_check_tasks_df(x, ...)
      self$graph <- dep_graph_create(tasks$package)
      tasks <- tasks[order(match(tasks$package, igraph::V(self$graph)$name)), ]
      rownames(tasks) <- NULL
      self$tasks <- tasks
    }
  ),
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
  print(x$tasks)
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
