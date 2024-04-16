install_packages <- function(
    ..., 
    lib.loc = .libPaths(),
    filters = NULL,
    logs_path = tempfile("reversecheck"),
    async = FALSE) {
  dir_create(dirname(logs_path))
  args <- list(...)
  f <- if (async) {
    callr::r_bg
  } else {
    callr::r
  }
  
  f(function(args, filters) {
    options(available_packages_filters = filters, timeout = 3600)
    # Installation
    do.call(utils::install.packages, args)
    
    # Verification
    length(
      find.package(
        args$pkgs, 
        lib.loc = args$lib,
        quiet = TRUE
      )
    ) > 0
  }, 
  args = list(args = args, filters = filters),
  libpath = lib.loc, 
  stdout = logs_path,
  stderr = logs_path)
}

installation_successful <- function(p) {
  !p$is_alive() && tryCatch(
    p$get_result(),
    error = function(e) FALSE
  )
}

installation_unsuccessful <- function(p) {
  !p$is_alive() && !tryCatch(
    p$get_result(),
    error = function(e) FALSE
  )
}