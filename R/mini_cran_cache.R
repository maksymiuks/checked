setup_minicran_cache_repo <- function(revdeps, reversecheck_dir, repos, type, cache_filters) {
  ap <- utils::available.packages(repos = repos, filters = cache_filters)
  
  pkgs_cache <- suppressWarnings(tryCatch(
    utils::available.packages(repos = repos)[, "Package"],
    error = function(e) {
      character(0)
    }))
  
  revdeps <- revdeps[revdeps$status == "awaiting"]
  
  revdeps_deps <- miniCRAN::pkgDep(
    pkg = revdeps$package, 
    availPkgs = ap,
    epos = NULL, 
    type = "source", 
    suggests = TRUE
  )
  
  revdeps_deps <- revdeps_deps[!revdeps_deps %in% pkgs_cache]
  ap <- ap[ap[, "Package"] %in% revdeps_deps, ] 
  ap_per_repo <- lapply(unique(ap[, "Repository"]), function(repo) {
    ap[ap[, "Repository"] == repo, c("Package", "Repository")]
  })
  
  lapply(ap_per_repo, function(pkgs) {
    repo <- if (endsWith(unique(pkgs[, "Repository"]), "/src/contrib")) {
      gsub("/src/contrib", "", unique(pkgs[, "Repository"]), fixed = TRUE)
    } else {
      unique(pkgs[, "Repository"])
    }
    miniCRAN::addPackage(
      pkgs = pkgs[, "Package"],
      path = get_reversecheck_cache_repo(reversecheck_dir),
      repos = repo,
      type = type,
      Rversion = R.version,
      deps = FALSE
    )
  })
  miniCRAN::updateRepoIndex(
    get_reversecheck_cache_repo(reversecheck_dir), 
    type = type, 
    Rversion = R.version
  )
  invisible(NULL)
}

preinstall_dependencies_cache <- function(reversecheck_dir, lib.loc) {
  pkgs <- utils::available.packages(repos = get_reversecheck_cache_repo(reversecheck_dir, TRUE))[, "Package"]
  
  for (pkg in pkgs) {
    install_packages(
      pkg,
      repos = get_reversecheck_cache_repo(reversecheck_dir, TRUE),
      lib.loc = lib.loc,
      logs_path = file.path(get_reversecheck_lib_logs(reversecheck_dir, "cache"), make.names(pkg), "subprocess.log"),
      keep_outputs = file.path(get_reversecheck_lib_logs(reversecheck_dir, "cache"), make.names(pkg))
    )
  }
}