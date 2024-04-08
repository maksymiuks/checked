get_revdeps_from_dir <- function(reversecheck_dir) {
  revdeps_dir <- path_revdeps(reversecheck_dir)
  revdeps <- list.dirs(revdeps_dir, recursive = FALSE, full.names = FALSE)
  
  data.frame(
    package = revdeps,
    status = vapply(revdeps, function(r) {
      get_revdep_status(reversecheck_dir, r)
    }, FUN.VALUE = character(1))
  )
}

revdeps <- function(pkg, reversecheck_dir, dependencies, repos, sampling) {
  
  recovered_revdeps <- get_revdeps_from_dir(reversecheck_dir)
  
  if (NROW(recovered_revdeps) > 0) {
    return(recovered_revdeps)
  }
  
  ap <- utils::available.packages(repos = repos)
  ap[is.na(ap)] <- ""
  
  revdeps_by_type <- lapply(dependencies, function(dep) {
    idx <- grepl(sprintf("(,| |\\n|^)(%s)(,| |\\n|$)", get_package_name(pkg)), ap[, dep])
    if (any(idx)) {
      data.frame(
        package = ap[idx, "Package", drop = TRUE],
        revdep_type = dep,
        status = "TODO",
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
  
  revdeps <- if (!is.null(sampling)) {
    sampling(revdeps)
  } else {
    revdeps
  }
  
  apply(revdeps, 1, function(x) {
    set_revdep_status(reversecheck_dir, x["package"], status = x["status"])
  })
  
  revdeps
}