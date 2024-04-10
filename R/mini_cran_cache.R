setup_minicran_cache_repo <- function(revdeps, reversecheck_dir, lib.loc, repos, type, filters) {
  ap <- utils::available.packages(repos = repos, filters = filters)
  
  pkgs_cache <- suppressWarnings(tryCatch(
    utils::available.packages(repos = path_cache_repo(reversecheck_dir, TRUE))[, "Package"],
    error = function(e) {
      character(0)
    }))
  
  pkgs_satisfied <- utils::installed.packages(lib.loc = lib.loc)[, "Package"]
  
  revdeps <- revdeps[revdeps$status == "TODO", ]
  
  if (NROW(revdeps) == 0) {
    return(invisible(NULL))
  }
  
  revdeps_deps <- miniCRAN::pkgDep(
    pkg = revdeps$package, 
    availPkgs = ap,
    epos = NULL, 
    type = "source", 
    suggests = TRUE,
    enhances = TRUE
  )
  
  revdeps_deps <- revdeps_deps[!revdeps_deps %in% c(pkgs_cache, pkgs_satisfied)]
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
      path = path_cache_repo(reversecheck_dir),
      repos = repo,
      type = type,
      Rversion = R.version,
      deps = FALSE
    )
  })
  miniCRAN::updateRepoIndex(
    path_cache_repo(reversecheck_dir), 
    type = type, 
    Rversion = R.version
  )
  invisible(NULL)
}

preinstall_dependencies_cache <- function(reversecheck_dir, lib.loc) {
  pkgs <- utils::available.packages(repos = path_cache_repo(reversecheck_dir, TRUE))[, "Package"]
  
  for (pkg in pkgs) {
    install_packages(
      pkg,
      repos = path_cache_repo(reversecheck_dir, TRUE),
      lib = path_lib(reversecheck_dir, "cache"),
      # root package has to be installable in native lib.loc hence we do not add cache
      lib.loc = lib.loc,
      logs_path = file.path(path_logs(reversecheck_dir, "cache"), make.names(pkg), "subprocess.log"),
      keep_outputs = file.path(path_logs(reversecheck_dir, "cache"), make.names(pkg))
    )
  }
}