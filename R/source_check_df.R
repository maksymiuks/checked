source_check_tasks_df <- function(path) {
  path <- vcapply(path, check_path_is_pkg_source, USE.NAMES = FALSE)
  package <- vcapply(path, get_package_name)
  alias <- if (is.null(names(path))) {
    paste(package, "(source)")
  } else {
    names(path)
  }
  version <- vcapply(path, get_package_version)
  
  df <- data.frame(
    alias = alias,
    version = version
  )
  
  df$package <- source_check_tasks_specs(package, path, alias)
  df$custom <- rep(list(custom_install_task_spec()), times = NROW(df))
  
  df
}


source_check_tasks_specs <- function(packages, path, aliases) {
  list_of_task_spec(mapply(
    function(p, path, a) {
      check_task_spec(
        alias = a,
        package = package_spec_source(name = p, path = path, repos = NULL),
        env = DEFAULT_R_CMD_CHECK_VARIABLES,
        check_args = DEFAULT_CHECK_ARGS,
        build_args = DEFAULT_BUILD_ARGS
      )
    },
    packages,
    path,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}
