STATUSES <- c("TODO", "PREPARING", "READY", "IN_PROGRESS_OLD", "IN_PROGRESS", "DONE")

get_revdep_status <- function(reversecheck_dir, revdep_name) {
  revdep_dir <- path_revdep(reversecheck_dir, revdep_name)
  STATUSES[STATUSES %in% list.files(revdep_dir, include.dirs = FALSE)]
}

set_revdep_status <- function(reversecheck_dir, revdep_name, status = STATUSES) {
  status <- match.arg(status, STATUSES)
  dir <- path_revdep(reversecheck_dir, revdep_name)
  unlink(
    list.files(
      dir, 
      pattern = paste0("^(", paste0(STATUSES, collapse = ")|("), ")$"), 
      full.names = TRUE)
  )
  
  path <- file.path(dir, status)
  dir_create(dirname(path))
  file.create(path)
}