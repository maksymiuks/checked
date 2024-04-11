reversecheck_initialize <- function(
    pkg,
    check_dir,
    lib.loc,
    dependencies,
    repos,
    dependencies_repos,
    minicran_type,
    minicran_filters) {
  pkg <- check_path_is_pkg_source(pkg)
  dependencies <- check_dependencies(dependencies)

  setup_reversecheck(check_dir)

  rdeps <- discover_revdeps_from_dir(check_dir)
  if (is.null(revdeps)) {
    rdeps <- revdeps(pkg, dependencies, repos)
    Map(set_revdep_status, check_dir, rdeps$package, rdeps$status)
  }

  install_pkg(pkg, check_dir, repos, lib.loc)
  setup_minicran_cache_repo(
    revdeps = revdeps,
    reversecheck_dir = check_dir,
    lib.loc = reversecheck_lib_loc(lib.loc, check_dir),
    repos = dependencies_repos,
    type = minicran_type,
    filters = minicran_filters
  )

  invisible(NULL)
}



install_pkg <- function(pkg, reversecheck_dir, repos, lib.loc) {
  name <- get_package_name(pkg)
  old <- path_lib(reversecheck_dir, "old")
  new <- path_lib(reversecheck_dir, "new")

  if (!is_package_installed(name, old)) {
    p_old <- install_packages(
      pkgs = name,
      lib = old,
      repos = repos,
      lib.loc = lib.loc,
      logs_path = file.path(path_logs(reversecheck_dir, "old"), "old_callr.log"),
      keep_outputs = file.path(path_logs(reversecheck_dir, "old")),
      async = TRUE
    )
  }

  if (!is_package_installed(name, new)) {
    p_new <- install_packages(
      pkgs = pkg,
      lib = new,
      lib.loc = lib.loc,
      repos = NULL,
      type = "source",
      logs_path = file.path(path_logs(reversecheck_dir, "new"), "new_callr.log"),
      keep_outputs = file.path(path_logs(reversecheck_dir, "new")),
      async = TRUE
    )
  }

  # Pause until both packages are installed
  while (TRUE) {
    if (exists("p_new") && installation_unsuccessful(p_new)) {
      stop(
        "Instalation of the development version of the package failed.\n ",
        "Check logs at ", file.path(path_logs(reversecheck_dir, "old"), "old_callr.log"),
        " - Aborting"
      )
    } else if (exists("p_old") && installation_unsuccessful(p_old)) {
      stop(
        "Instalation of the development version of the package failed.\n ",
        "Check logs at ", file.path(path_logs(reversecheck_dir, "new"), "new_callr.log"),
        " - Aborting"
      )
    } else if (is_package_installed(name, old) && is_package_installed(name, new)) {
      break()
    }
    Sys.sleep(1)
  }
}
