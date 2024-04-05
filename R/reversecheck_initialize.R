reversecheck_initialize <- function(
  pkg,
  reversecheck_dir,
  lib.loc,
  dependencies,
  cache,
  repos,
  dependencies_repos,
  sampling,
  cache_type,
  cache_filters
) {
  
  revdeps <- revdeps(pkg, reversecheck_dir, dependencies, repos, sampling)

  install_pkg(pkg, reversecheck_dir, lib.loc)
  setup_minicran_cache_repo(revdeps, reversecheck_dir, repos, cache_type, cache_filters)
  
  if (cache == "preinstall") 
    preinstall_dependencies_cache(reversecheck_dir, lib.loc)
  
  invisible(NULL)
}



install_pkg <- function(pkg, reversecheck_dir, lib.loc) {
  name <- get_package_name(pkg)
  old <- get_reversecheck_lib(reversecheck_dir, "old")
  new <- get_reversecheck_lib(reversecheck_dir, "new")
  
  if (!is_package_installed(name, old)) {
    install_packages(
      pkgs = name, 
      lib = get_reversecheck_lib(reversecheck_dir, "old"), 
      lib.loc = lib.loc, 
      logs_path = file.path(get_reversecheck_lib_logs(reversecheck_dir, "old"), "old.log"),
      keep_outputs = file.path(get_reversecheck_lib_logs(reversecheck_dir, "old"))
    )
  }

  if (!is_package_installed(name, new)) {
    install_packages(
      pkgs = pkg, 
      lib = get_reversecheck_lib(reversecheck_dir, "new"), 
      lib.loc = lib.loc,
      repos = NULL,
      type = "source",
      logs_path = file.path(get_reversecheck_lib_logs(reversecheck_dir, "new"), "new.log"),
      keep_outputs = file.path(get_reversecheck_lib_logs(reversecheck_dir, "new"))
    )
  }
}
