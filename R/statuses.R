STATUSES <- c("TODO", "PREPARING", "READY", "IN_PROGRESS_OLD", "IN_PROGRESS", "DONE")

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
