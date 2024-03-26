install_packages_cached <- function(...) {
  
}


install_packages <- function(
    ..., 
    lib.loc = .libPaths(), 
    logs_path = tempfile("reversecheck")) {
  
  args <- list(...)
  callr::r(function(args) {
    do.call(utils::install.packages, args)
  }, 
  args = list(args = args),
  libpath = lib.loc, 
  stdout = logs_path,
  stderr = logs_path)
}
