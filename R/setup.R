setup_reversecheck <- function(path, cache) {
  if (pre_clear) unlink(path, recursive = TRUE, force = TRUE) 
  
  dir_create(path)
  dir_create(path_cache_repo(path))
  
  if (cache != "none") dir_create(path_lib(path, "cache"))
  dir_create(path_lib(path, "old"))
  dir_create(path_lib(path, "new"))
  
  if (cache != "none") dir_create(path_logs(path, "cache"))
  dir_create(path_logs(path, "old"))
  dir_create(path_logs(path, "new"))
  dir_create(path_revdeps(path))
}

path_default <- function() {
  file.path(tempdir(), utils::packageName())
}

path_cache_repo <- function(path, repos = FALSE) {
  path <- normalizePath(file.path(path, "repo"), mustWork = FALSE)
  if (repos) {
    paste0("file://", path)
  } else {
    path
  }
}

path_lib <- function(path, lib = c("cache", "new", "old")) {
  lib <- match.arg(lib, c("cache", "new", "old"))
  normalizePath(file.path(path, "libs", paste0("R_REVERSECHECK_LIB_", toupper(lib))), mustWork = FALSE)
}

path_logs <- function(path, lib = c("cache", "new", "old")) {
  lib <- match.arg(lib, c("cache", "new", "old"))
  normalizePath(file.path(path, "libs", "logs", lib), mustWork = FALSE)
}

path_revdeps <- function(path) {
  normalizePath(file.path(path, "revdeps"), mustWork = FALSE)
}

path_revdep <- function(path, revdep) {
  normalizePath(file.path(path_revdeps(path), revdep), mustWork = FALSE)
}


path_revdep_lib <- function(path, revdep) {
  normalizePath(file.path(path_revdep(path, revdep), paste0("R_REVERSECHECK_LIB_", toupper(revdep))), mustWork = FALSE)
}

path_revdep_logs <- function(path, revdep) {
  normalizePath(file.path(path_revdep(path, revdep), "logs"), mustWork = FALSE)
}
