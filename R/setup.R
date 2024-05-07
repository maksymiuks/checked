path_default <- function() {
  file.path(tempdir(), utils::packageName())
}

path_lib <- function(path) {
  dir_create(p <- file.path(path, "lib"))
  normalizePath(p)
}

path_package_install_log <- function(path, package, name = "lib") {
  dir_create(p <- file.path(path_logs(path), name))
  normalizePath(file.path(p, sprintf("%s.log", package)), mustWork = FALSE)
}

path_logs <- function(path) {
  dir_create(p <- file.path(path, "logs"))
  normalizePath(p)
}

path_check_output <- function(path) {
  # holdplacer
}
