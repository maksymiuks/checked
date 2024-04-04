install_package_cached <- function(pkg, reversecheck_dir, lib, lib.loc = .libPaths(), ...) {
  checkmate::assert_string(pkg)
  cache <- get_reversecheck_lib(reversecheck_dir, "cache")
  ip_cache <- utils::installed.packages(lib.loc = cache)

  if (pkg %in% ip_cache[, "Package"]) {
    file.symlink(
      from = normalizePath(file.path(cache, pkg)),
      to = normalizePath(file.path(lib, pkg), mustWork = FALSE)
    )
  } else {
    dir.create(temp_logs_dir <- file.path(tempdir(), "reversecheck_temp_logs"))
    callr_temp_log_file <- file.path(temp_logs_dir, paste0(pkg, "_callr.log"))

    install_packages(
      pkgs = pkg,
      lib = lib,
      keep_outputs = temp_logs_dir,
      ...,
      lib.loc = lib.loc,
      logs_path = callr_temp_log_file
    )

    dir.create(cached_pkg_log_dir <- file.path(
      get_reversecheck_lib_logs(reversecheck_dir, "cache"),
      make.names(pkg)
    ))

    file.copy(file.path(lib, pkg), cache, overwrite = TRUE, recursive = TRUE)
    file.copy(callr_temp_log_file, cached_pkg_log_dir)
    suppressWarnings(
      # Suppressing warnings as this particular file might not exists
      file.copy(file.path(temp_logs_dir, paste0(pkg, ".out")), cached_pkg_log_dir)
    )
  }
}

install_packages <- function(
    ...,
    lib.loc = .libPaths(),
    logs_path = tempfile("reversecheck")) {
  args <- list(...)
  callr::r(
    function(args) {
      do.call(utils::install.packages, args)
    },
    args = list(args = args),
    libpath = lib.loc,
    stdout = logs_path,
    stderr = logs_path
  )
}
