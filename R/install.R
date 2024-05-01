#' @importFrom R6 R6Class
#' @importFrom callr r_process
install_packages_process <- R6::R6Class(
  "install_package_process",
  inherit = callr::r_process,
  public = list(
    initialize = function(pkgs, ..., lib = .libPaths(), libpaths = .libPaths(), log) {
      private$package <- pkgs
      private$callr_r_bg(
        function(...) install.packages(...),
        args = list(pkgs, ..., lib = lib),
        libpath = libpaths,
        stdout = log,
        stderr = "2>&1"
      )
    },
    set_finalizer = function(callback) {
      private$finalize_callback <- callback
    },
    finalize = function() {
      if (is.function(f <- private$finalize_callback)) f(self)
      if ("finalize" %in% ls(super)) super$finalize()
    }
  ),
  private = list(
    options = NULL,
    package = NULL,
    finalize_callback = NULL,
    callr_r_bg = function(...) {
      # default formal argument values
      options <- formals(callr::r_bg)
      options <- options[vlapply(options, `!=`, bquote())]
      options <- lapply(options, eval, envir = asNamespace("callr"))

      # ellipsis arguments
      dots <- match.call(callr::r_bg, expand.dots = TRUE)[-1]
      dots <- lapply(dots, eval, envir = parent.frame())
      options[names(dots)] <- dots

      # other things set internally in callr::r_bg
      options$extra <- list()
      options$load_hook <- ""

      private$options <- options
      super$initialize(options = options)
    }
  )
)
