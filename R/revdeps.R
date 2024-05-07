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
