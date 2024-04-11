#' @importFrom R6 R6Class
#' @importFrom callr r_process
prepare_filesystem_process <- R6::R6Class(
  "revcheck_process",
  inherit = callr::r_process,
  public = list(
    initialize = function(revdep, root, reversecheck_dir, repos, lib.loc, ...) {
      options <- lapply(formals(callr::r_bg), function(x) {
        tryCatch(
          eval(x, envir = asNamespace("callr")),
          error = function(e) NULL
        )
      })
      params <- list(...)
      options <- as.list(modifyList(options, params))
      
      options$func <- function(revdep, root, reversecheck_dir, repos, lib.loc) {
        
        get_revdep_source(revdep, repos, path_revdep(reversecheck_dir, revdep))
        dir_create(revdep_lib_path <- path_revdep_lib(reversecheck_dir, revdep))
        deps <- miniCRAN::pkgDep(
          revdep,
          availPkgs = utils::installed.packages(lib.loc = lib.loc),
          repos = NULL,
          suggests = TRUE,
          enhances = TRUE
        )
        # skip reversecheck root and the revdep itself
        deps <- deps[!deps %in% c(revdep, root)]
        
        symlink_or_copy(
          from = find.package(deps, lib.loc = lib.loc),
          to = revdep_lib_path
        )
        
        set_revdep_status(reversecheck_dir, revdep, status = "READY")
        
        TRUE
      }
      
      options$args <- list(
        revdep = revdep,
        root = root,
        reversecheck_dir = reversecheck_dir, 
        repos = repos,
        lib.loc = lib.loc
      )
      dir_create(dirname(logs_path <- file.path(path_revdep_logs(reversecheck_dir, revdep), "preparation.log")))
      options$package <- "reversecheck"
      options$libpath <- .libPaths()
      options$stdout <- options$stderr  <- logs_path
      private$options <- options
      super$initialize(options)
    }
  ),
  private = list(
    options = list(),
    source_url = NULL
  )
)


get_revdep_source <- function(revdep, repos, destdir) {
  ap <- utils::available.packages(repos = repos)
  pkg <- ap[revdep, ]
  archive_url <- sprintf(
    "%s/%s_%s.tar.gz",
    pkg["Repository"],
    pkg["Package"],
    pkg["Version"]
  )
  
  bn <- basename(archive_url)
  dir_create(tmpdir <- file.path(tempfile("reversecheck_"), revdep))
  destfile <- file.path(tmpdir, bn)
  download.file(archive_url, destfile = destfile)
  untar(destfile, exdir = normalizePath(destdir))
  archive_url
}
