#' @export
reversecheck <- function(pkg = "./",
                         reversecheck_dir = "./reversecheck",
                         lib.loc = .libPaths(),
                         dependencies = TRUE,
                         cache = c("preinstall", "standard", "none"),
                         repos = getOption("repos"),
                         dependencies_repos = repos,
                         timeout = as.difftime(60, units = "mins"),
                         sampling = NULL,
                         rcmdcheck_params = reversecheck_deafult_rcmd_params(),
                         pre_clear = FALSE,
                         cache_type = "source",
                         cache_filters = NULL,
                         ...) {
  
  pkg <- check_input_pkg(pkg)
  dependencies <- check_input_dependencies(dependencies)
  cache <- match.arg(cache, c("preinstall", "standard", "none"))
  checkmate::assert_class(timeout, "difftime")
  checkmate::assert_function(sampling, null.ok = TRUE)
  checkmate::assert_character(rcmdcheck_params, null.ok = TRUE)
  
  setup_reversecheck(reversecheck_dir, pre_clear, cache)
  
  reversecheck_initialize(
    pkg = pkg,
    reversecheck_dir = reversecheck_dir,
    lib.loc = lib.loc,
    dependencies = dependencies,
    cache = cache,
    repos = repos,
    dependencies_repos = dependencies_repos,
    sampling = sampling,
    cache_type = cache_type,
    cache_filters = cache_filters,
    ...
  )
  reversecheck_check(
    pkg = pkg,
    reversecheck_dir = reversecheck_dir,
    timeout = timeout,
    rcmdcheck_params = rcmdcheck_params,
    ...
  )
  
  reversecheck_report()
  
  invisible(NULL)
}


reversecheck_deafult_rcmd_params <- function() {
  list()
}