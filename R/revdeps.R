discover_revdeps_from_dir <- function(path) {
  revdeps_dir <- path_revdeps(path)
  revdeps <- list.dirs(revdeps_dir, recursive = FALSE, full.names = FALSE)

  if (length(revdeps) == 0L) {
    return(NULL)
  }

  data.frame(
    package = revdeps,
    status = vcapply(revdeps, get_revdep_status, path = path)
  )
}

revdeps <- function(pkg, reversecheck_dir, dependencies, repos) {
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
  apply(revdeps, 1, function(x) {
    set_revdep_status(reversecheck_dir, x["package"], status = x["status"])
  })

  revdeps
}
