#' @importFrom R6 R6Class
#' @importFrom callr r_process
install_packages_process <- R6::R6Class(
  "install_packages_process",
  inherit = callr::r_process,
  public = list(
    initialize = function(...,
                          lib.loc = .libPaths(),
                          filters = NULL,
                          logs_path = tempfile("reversecheck")) {
      options <- lapply(formals(callr::r_bg), function(x) {
        tryCatch(
          eval(x, envir = asNamespace("callr")),
          error = function(e) {
            NULL
          }
        )
      })

      args <- as.list(match.call(utils::install.packages, expand.dots = TRUE)[-1])
      args <- lapply(args, eval, envir = parent.frame())

      ip_args <- args[names(args) %in% names(formals(utils::install.packages))]

      options$func <- function(args, filters) {
        options(available_packages_filters = filters, timeout = 3600)
        do.call(utils::install.packages, args)
      }

      options$args <- list(
        args = ip_args,
        filters = filters
      )

      options$libpath <- lib.loc
      dir_create(dirname(options$stdout <- options$stderr <- logs_path))
      private$options <- options
      private$pkg <- ip_args$pkgs
      private$lib <- ip_args$lib
      super$initialize(options)
    },
    get_installation_status = function() {
      if (self$is_alive()) {
        NA
      } else {
        is_package_installed(get_package_name(private$pkg), private$lib)
      }
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
    options = list(),
    pkg = NULL,
    lib = NULL,
    finalize_callback = NULL
  )
)
