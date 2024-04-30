STATUSES <- c("TODO", "PREPARING", "READY", "IN_PROGRESS_OLD", "IN_PROGRESS", "DONE")

#' @export
get_process_status <- function(x) {
  UseMethod("get_process_status")
}

#' @export
get_process_status.factor <- function(x) {
  x
}

#' @export
get_process_status.NULL <- function(x) {
  STATUS$pending
}

#' @export
get_process_status.revdep_process <- function(x) {
  if (x$is_alive()) STATUS$`in progress` else STATUS$done
}

get_revdep_status <- function(path, pkg) {
  dir <- path_revdep(path, pkg)
  STATUSES[STATUSES %in% list.files(dir, include.dirs = FALSE)]
}

set_revdep_status <- function(path, pkg, status = STATUSES) {
  status <- match.arg(status, STATUSES)
  dir <- path_revdep(path, pkg)
  unlink(
    list.files(
      dir,
      pattern = paste0("^(", paste0(STATUSES, collapse = ")|("), ")$"),
      full.names = TRUE
    )
  )

  path <- file.path(dir, status)
  dir_create(dirname(path))
  file.create(path)
}
