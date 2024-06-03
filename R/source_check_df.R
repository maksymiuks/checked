source_check_tasks_df <- function(path) {
  alias <- names(path)
  path <- vcapply(path, check_path_is_pkg_source)
  package <- vcapply(path, get_package_name)
  ap <- available.packages(repos = repos)
  version <- ap[package, "Version"]
  
  df <- data.frame(
    alias = revdeps,
    version = version
  )
  
  df$package <- rev_dep_check_tasks_specs(revdeps, repos, df_dev$alias, "new")
  df_dev$custom <- rep(list(custom_install_task_spec()), times = NROW(df_dev))
  df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
  df_rel$package <- rev_dep_check_tasks_specs(revdeps, repos, df_rel$alias, "old")
  df_rel$custom <- rep(list(custom_install_task_spec()), times = NROW(df_dev))
  
  idx <- rep(seq_len(nrow(df_rel)), each = 2) + c(0, nrow(df_rel))
  rbind(df_dev, df_rel)[idx, ]
}


rev_dep_check_tasks_specs <- function(packages, repos, aliases = packages, revdep, db = available.packages(repos = repos)) {
  db <- available.packages(repos = repos)
  mapply(function(p, a) {
    revdep_check_task_spec(
      name = p,
      alias = a,
      path = get_package_source(p, db = db),
      repos = repos,
      env = DEFAULT_R_CMD_CHECK_VARIABLES,
      check_args = DEFAULT_CHECK_ARGS,
      build_args = DEFAULT_BUILD_ARGS,
      revdep = revdep
    )
  }, packages, aliases, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}