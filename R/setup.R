setup_reversecheck <- function(path = path_default(), force = FALSE, cache) {
  unlink(path, recursive = TRUE, force = force)

  dir_create(path)
  db_setup(path_db(path))
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

path_db <- function(path) {
  name <- paste0(utils::packageName(), "_db.sql")
  file.path(path, name)
}

path_cache_repo <- function(path, repos = FALSE) {
  if (repos) {
    paste0("file://", file.path(path, "repo"))
  } else {
    file.path(path, "repo")
  }
}

path_lib <- function(path, lib = c("cache", "new", "old")) {
  lib <- match.arg(lib)
  file.path(path, "lib", lib)
}

path_logs <- function(path, lib = c("cache", "new", "old")) {
  lib <- match.arg(lib)
  file.path(path, "lib", "logs", lib)
}

path_revdeps <- function(path) {
  file.path(path, "revdeps")
}

path_revdep <- function(path, revdep) {
  file.path(path_revdeps(path), revdep)
}

path_revdep_lib <- function(path, revdep) {
  file.path(path_revdep(path, revdep), "lib")
}

path_revdep_logs <- function(path, revdep) {
  file.path(path_revdep(path, revdep), "logs")
}
