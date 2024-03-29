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
  was_initialized <- db_was_initialized(reversecheck_dir)
  revdeps <- if (was_initialized) {
    db_get_revdeps(reversecheck_dir)
  } else {
    db_insert_revdeps(
      get_revdeps(pkg, dependencies, repos, sampling),
      reversecheck_dir
    )
  }

  install_pkg(pkg, reversecheck_dir, lib.loc)
  setup_minicran_cache_repo(revdeps, reversecheck_dir, repos, cache_type, cache_filters)
  
  if (cache == "preinstall") 
    preinstall_dependencies_cache(reversecheck_dir, lib.loc)
  
  invisible(NULL)
}

get_revdeps <- function(pkg, dependencies, repos, sampling) {
  ap <- utils::available.packages(repos = repos)
  ap[is.na(ap)] <- ""
  
  revdeps_by_type <- lapply(dependencies, function(dep) {
    idx <- grepl(sprintf("(,| |\\n|^)(%s)(,| |\\n|$)", get_package_name(pkg)), ap[, dep])
    if (any(idx)) {
      data.frame(
        package = ap[idx, "Package", drop = TRUE],
        revdep_type = dep,
        status = "awaiting",
        row.names = NULL
      )
    } else {
      data.frame(
        package = character(0),
        revdep_type = character(0),
        status = character(0),
        row.names = NULL
      )
    }
  })
  
  revdeps <- do.call(rbind, revdeps_by_type)

  if (!is.null(sampling)) {
    sampling(revdeps)
  } else {
    revdeps
  }
}

install_pkg <- function(pkg, reversecheck_dir, lib.loc) {
  old <- get_reversecheck_lib(reversecheck_dir, "old")
  new <- get_reversecheck_lib(reversecheck_dir, "new")
  
  install_packages(
    pkgs = get_package_name(pkg), 
    lib = get_reversecheck_lib(reversecheck_dir, "old"), 
    lib.loc = lib.loc, 
    logs_path = file.path(get_reversecheck_lib_logs(reversecheck_dir, "old"), "old.log"),
    keep_outputs = file.path(get_reversecheck_lib_logs(reversecheck_dir, "old"))
  )
  
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
