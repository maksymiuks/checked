reversecheck_initialize <- function(pkg, reversecheck_dir, lib.loc, dependencies,
                                    repos, sampling, ...) {
  revdeps <- revdeps(pkg, reversecheck_dir, dependencies, repos, sampling)

  install_pkg(pkg, reversecheck_dir, repos, lib.loc)

  invisible(NULL)
}



install_pkg <- function(pkg, reversecheck_dir, repos, lib.loc) {
  name <- get_package_name(pkg)
  old <- path_lib(reversecheck_dir, "old")
  new <- path_lib(reversecheck_dir, "new")

  if (!is_package_installed(name, old)) {
    p_old <- install_packages_process$new(
      pkgs = name,
      lib = old,
      repos = repos,
      lib.loc = lib.loc,
      logs_path = file.path(path_logs(reversecheck_dir, "old"), "old_callr.log"),
      keep_outputs = file.path(path_logs(reversecheck_dir, "old"))
    )
  }

  if (!is_package_installed(name, new)) {
    p_new <- install_packages_process$new(
      pkgs = pkg,
      lib = new,
      lib.loc = lib.loc,
      repos = NULL,
      type = "source",
      logs_path = file.path(path_logs(reversecheck_dir, "new"), "new_callr.log"),
      keep_outputs = file.path(path_logs(reversecheck_dir, "new"))
    )
  }

  # Pause until both packages are installed
  while (TRUE) {
    if (exists("p_new") && isFALSE(p_new$get_installation_status())) {
      stop(
        "Instalation of the development version of the package failed.\n ",
        "Check logs at ", file.path(path_logs(reversecheck_dir, "old"), "old_callr.log"),
        " - Aborting"
      )
    } else if (exists("p_old") && isFALSE(p_old$get_installation_status())) {
      stop(
        "Instalation of the release version of the package failed.\n ",
        "Check logs at ", file.path(path_logs(reversecheck_dir, "new"), "new_callr.log"),
        " - Aborting"
      )
    } else if (is_package_installed(name, old) && is_package_installed(name, new)) {
      break()
    }
    Sys.sleep(1)
  }
}
