rev_dep_check_tasks_df <- function(path, repos = getOption("repos")) {
  ap <- available.packages(repos = repos)
  path <- check_path_is_pkg_source(path)
  package <- get_package_name(path)
  package_v <- ap[package, "Version"]
  revdeps <- tools::package_dependencies(package, reverse = TRUE)[[1]]
  version <- ap[revdeps, "Version"]
  df_dev <- df_rel <- data.frame(
    alias = revdeps,
    version = version
  )

  df_dev$alias <- paste0(df_dev$alias, " (dev)")
  df_dev$package <- rev_dep_check_tasks_specs(revdeps, repos, df_dev$alias, "new")
  df_dev$custom <- rep(list(custom_install_task_spec(
    alias = paste0(package, " (dev)"),
    package = package_spec_source(name = package, path = path),
    type = "source"
  )), times = NROW(df_dev))

  df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
  df_rel$package <- rev_dep_check_tasks_specs(revdeps, repos, df_rel$alias, "old")
  df_rel$custom <- rep(list(custom_install_task_spec()), times = NROW(df_dev))

  idx <- rep(seq_len(nrow(df_rel)), each = 2) + c(0, nrow(df_rel))
  df <- rbind(df_dev, df_rel)[idx, ]

  df$package <- list_of_task_spec(df$package)
  df$custom <- list_of_task_spec(df$custom)
  df
}

rev_dep_check_tasks_specs <- function(packages, repos, aliases, revdep) {
  list_of_task_spec(mapply(
    function(p, a) {
      revdep_check_task_spec(
        alias = a,
        package = package_spec(name = p, path = NULL, repos = repos),
        env = DEFAULT_R_CMD_CHECK_VARIABLES,
        check_args = DEFAULT_CHECK_ARGS,
        build_args = DEFAULT_BUILD_ARGS,
        revdep = revdep
      )
    },
    packages,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}
