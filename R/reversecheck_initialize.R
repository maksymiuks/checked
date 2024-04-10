reversecheck_initialize <- function(pkg, reversecheck_dir, lib.loc, dependencies,
                                    repos, dependencies_repos, sampling, minicran_type,
                                    minicran_filters) {
  
  revdeps <- revdeps(pkg, reversecheck_dir, dependencies, repos, sampling)

  install_pkg(pkg, reversecheck_dir, repos, lib.loc)
  setup_minicran_cache_repo(
    revdeps = revdeps, 
    reversecheck_dir = reversecheck_dir, 
    lib.loc = reversecheck_lib_loc(lib.loc, reversecheck_dir), 
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
    install_packages(
      pkgs = name,
      lib = old,
      repos = repos,
      lib.loc = lib.loc,
      logs_path = file.path(path_logs(reversecheck_dir, "old"), "old_callr.log"),
      keep_outputs = file.path(path_logs(reversecheck_dir, "old"))
    )
  }

  if (!is_package_installed(name, new)) {
    install_packages(
      pkgs = pkg, 
      lib = new, 
      lib.loc = lib.loc,
      repos = NULL,
      type = "source",
      logs_path = file.path(path_logs(reversecheck_dir, "new"), "new_callr.log"),
      keep_outputs = file.path(path_logs(reversecheck_dir, "new"))
    )
  }
}
