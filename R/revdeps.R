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
  df_dev$package <- rev_dep_check_tasks_specs(revdeps, repos, df_dev$alias, "new", db = ap)
  df_dev$custom <- rep(list(custom_install_task_spec(
    alias = paste0(package, " (dev)"),
    name = package,
    path = path,
    type = "source"
  )), times = NROW(df_dev))
  
  df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
  df_rel$package <- rev_dep_check_tasks_specs(revdeps, repos, df_rel$alias, "old", db = ap)
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

get_package_source <- function(revdep, repos, db = NULL, destdir = NULL) {
  if (is.null(db)) {
    db <- utils::available.packages(repos = repos)
  }
  pkg <- db[revdep, ]
  archive_url <- sprintf(
    "%s/%s_%s.tar.gz",
    pkg["Repository"],
    pkg["Package"],
    pkg["Version"]
  )
  
  if (!is.null(destdir)) {
    fetch_package_source(archive_url, destdir)
  } else {
    archive_url
  }
}

fetch_package_source <- function(archive_url, destdir) {
  bn <- basename(archive_url)
  destfile <- file.path(destdir, bn)
  if (!file.exists(destfile)) {
    utils::download.file(archive_url, destfile = destfile, quiet = TRUE)
  }
  destfile
}