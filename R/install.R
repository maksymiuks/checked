install_packages <- function(
    ..., 
    lib.loc = .libPaths(), 
    logs_path = tempfile("reversecheck"),
    async = FALSE) {
  dir_create(dirname(logs_path))
  args <- list(...)
  f <- if (async) {
    callr::r_bg
  } else {
    callr::r
  }
  
  f(function(args) {
    do.call(utils::install.packages, args)
  }, 
  args = list(args = args),
  libpath = lib.loc, 
  stdout = logs_path,
  stderr = logs_path)
}
