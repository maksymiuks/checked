rev_dep_check_tasks_df <- function(path, repos = getOption("repos")) {
  path <- check_path_is_pkg_source(path)
  package <- get_package_name(path)
  package_v <- available.packages(repos = repos)[package, "Version"]
  revdeps <- tools::package_dependencies(package, reverse = TRUE)[[1]]
  version <- available.packages()[revdeps, "Version"]
  
  df_dev <- df_rel <- data.frame(
    alias = revdeps,
    version = version
  )
  
  df_dev$alias <- paste0(df_dev$alias, " (dev)")
  df_dev$package <- rev_dep_check_tasks(revdeps, repos, df_dev$alias)
  df_dev$custom <- rep(list(reversecheck_package(
    alias = paste0(package, " (dev)"),
    name = package,
    path = path
  )), times = NROW(df_dev))
  
  
  df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
  df_rel$package <- rev_dep_check_tasks(revdeps, repos, df_rel$alias)
  df_rel$custom <- rep(list(reversecheck_package()), times = NROW(df_dev))
  
  
  df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
  idx <- rep(seq_len(nrow(df_rel)), each = 2) + c(0, nrow(df_rel))
  rbind(df_dev, df_rel)[idx, ]
}

rev_dep_check_tasks <- function(packages, repos, aliases = packages) {
  db <- available.packages(repos = repos)
  mapply(function(p, a) {
    reversecheck_package(
      name = p,
      alias = a,
      path = get_package_source(p, db = db),
      type = "source",
      repos = repos
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
    bn <- basename(archive_url)
    dir_create(tmpdir <- file.path(tempfile("reversecheck_"), revdep))
    destfile <- file.path(tmpdir, bn)
    utils::download.file(archive_url, destfile = destfile)
    utils::untar(destfile, exdir = normalizePath(destdir))
  } else {
    archive_url
  }
}
