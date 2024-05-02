#' @importFrom R6 R6Class
#' @importFrom rcmdcheck rcmdcheck_process
mock_process <- R6::R6Class(
  "mock_process",
  inherit = callr::r_process,
  public = list(
    initialize = function(n, ..., class = c()) {
      options <- formals(callr::r_bg)
      options <- options[vlapply(options, `!=`, bquote())]
      options <- lapply(options, eval, envir = getNamespace("callr"))
      options$extra <- list()
      options$load_hook <- callr:::default_load_hook()
      options$func <- function(n) Sys.sleep(n)
      options$args <- list(n = n)
      class(self) <- c(class, class(self))
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
