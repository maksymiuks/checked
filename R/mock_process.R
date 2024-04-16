#' @importFrom R6 R6Class
#' @importFrom rcmdcheck rcmdcheck_process
mock_process <- R6::R6Class(
  "mock_process",
  inherit = callr::r_process,
  public = list(
    initialize = function(n, ...) {
      library(callr)
      options <- formals(callr::r_bg)
      options <- options[vlapply(options, `!=`, bquote())]
      options <- lapply(options, eval)
      options$extra <- list()
      options$load_hook <- callr:::default_load_hook()
      options$func <- function(n) Sys.sleep(n)
      options$args <- list(n = n)
      super$initialize(options = options)
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
    finalize_callback = NULL
  )
)
