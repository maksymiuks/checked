setup_reversecheck <- function(reversecheck_dir, pre_clear, cache) {
  if (pre_clear) unlink(reversecheck_dir, recursive = TRUE, force = TRUE) 
  
  dir_create(reversecheck_dir)
  dir_create(get_reversecheck_cache_repo(reversecheck_dir))
  if (cache != "none") dir_create(get_reversecheck_lib(reversecheck_dir, "cache"))
  dir_create(get_reversecheck_lib(reversecheck_dir, "old"))
  dir_create(get_reversecheck_lib(reversecheck_dir, "new"))
  if (cache != "none") dir_create(get_reversecheck_lib_logs(reversecheck_dir, "cache"))
  dir_create(get_reversecheck_lib_logs(reversecheck_dir, "old"))
  dir_create(get_reversecheck_lib_logs(reversecheck_dir, "new"))
  dir_create(get_reversecheck_revdeps_dir(reversecheck_dir))
}

get_reversecheck_cache_repo <- function(reversecheck_dir, repos = FALSE) {
  path <- normalizePath(file.path(reversecheck_dir, "repo"), mustWork = FALSE)
  if (repos) {
    paste0("file://", path)
  } else {
    path
  }
}

get_reversecheck_lib <- function(reversecheck_dir, lib = c("cache", "new", "old")) {
  lib <- match.arg(lib)
  normalizePath(file.path(reversecheck_dir, "libs", paste0("R_REVERSECHECK_LIB_", toupper(lib))), mustWork = FALSE)
}

get_reversecheck_lib_logs <- function(reversecheck_dir, lib = c("cache", "new", "old")) {
  lib <- match.arg(lib)
  normalizePath(file.path(reversecheck_dir, "libs", "logs", lib), mustWork = FALSE)
}

get_reversecheck_revdeps_dir <- function(reversecheck_dir) {
  normalizePath(file.path(reversecheck_dir, "revdeps"), mustWork = FALSE)
}

get_reversecheck_revdep_dir <- function(reversecheck_dir, revdep) {
  normalizePath(file.path(get_reversecheck_revdeps_dir(reversecheck_dir), revdep), mustWork = FALSE)
}


get_reversecheck_revdep_lib <- function(reversecheck_dir, revdep) {
  normalizePath(file.path(get_reversecheck_revdep_dir(reversecheck_dir, revdep), paste0("R_REVERSECHECK_LIB_", toupper(revdep))), mustWork = FALSE)
}

get_reversecheck_revdep_logs_dir <- function(reversecheck_dir, revdep) {
  normalizePath(file.path(get_reversecheck_revdep_dir(reversecheck_dir, revdep), "logs"), mustWork = FALSE)
}


