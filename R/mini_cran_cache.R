setup_minicran_cache_repo <- function(revdeps, reversecheck_dir, lib.loc, repos, type, filters) {
  ap <- utils::available.packages(repos = repos, filters = filters)
  
  pkgs_cache <- suppressWarnings(tryCatch(
    utils::available.packages(repos = path_cache_repo(reversecheck_dir, TRUE), type = minicran_pkg_type(type))[, "Package"],
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
    type = type, 
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

minicran_pkg_type <- function(type) {
  if (.Platform$pkgType %in% type) {
    "binary"
  } else if ("source" %in% type) {
    "source"
  } else {
    stop("Specified minicran_type does not allow proper miniCRAN repository setup. ",
         sprintf("Consider using `%s`, `source` or both", .Platform$pkgType))
  }
}

merge_minicran_db <- function(repos, type, lib.loc) {
  ap <- available.packages(repos = repos, type = type)
  ip <- installed.packages(lib.loc = lib.loc)
  
  rbind(
    ip[, intersect(colnames(ip), colnames(ap))],
    ap[, intersect(colnames(ip), colnames(ap))]
  )
  
}