setup_reversecheck <- function(path) {
  dir_create(path)
  dir_create(path_lib(path, "cache"))
  dir_create(path_lib(path, "old"))
  dir_create(path_lib(path, "new"))

  dir_create(path_logs(path, "cache"))
  dir_create(path_logs(path, "old"))
  dir_create(path_logs(path, "new"))
  dir_create(path_revdeps(path))
}

path_default <- function() {
  file.path(tempdir(), utils::packageName())
}

path_libs <- function(path) {
  if (!dir.exists(p <- file.path(path, "lib"))) {
    dir.create(p, recursive = TRUE)
  }
  normalizePath(p)
}

path_package_libs <- function(path, packages = NULL, create = FALSE) {
  libs <- path_libs(path)
  if (is.null(packages)) packages <- list.files(libs)
  paths <- file.path(libs, packages)
  if (create && (i <- !dir.exists(paths))) {
    for (path in paths[i]) dir.create(paths[[i]], recursive = TRUE)
  }
  normalizePath(file.path(libs, packages), mustWork = FALSE)
}

path_package_install_log <- function(path, package) {
  path <- file.path(path_package_libs(path, package), "install.log")
  normalizePath(path, mustWork = FALSE)
}

path_logs <- function(path, name) {
  lib <- match.arg(lib, c("cache", "new", "old"))
  normalizePath(file.path(path, "libs", "logs", lib), mustWork = FALSE)
}

path_revdeps <- function(path) {
  normalizePath(file.path(path, "revdeps"), mustWork = FALSE)
}

path_check <- function(path, name) {
  path <- file.path(path, "checks", name)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  normalizePath(file.path(path, "checks", name), mustWork = FALSE)
}

path_revdep_lib <- function(path, revdep) {
  normalizePath(file.path(path_check(path, revdep), paste0("R_REVERSECHECK_LIB_", toupper(revdep))), mustWork = FALSE)
}

path_revdep_logs <- function(path, revdep) {
  normalizePath(file.path(path_check(path, revdep), "logs"), mustWork = FALSE)
}
