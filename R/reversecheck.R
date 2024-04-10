#' @export
reversecheck <- function(pkg = "./",
                         reversecheck_dir = "./reversecheck",
                         lib.loc = .libPaths(),
                         dependencies = TRUE,
                         n_childs = 1L,
                         repos = getOption("repos"),
                         dependencies_repos = repos,
                         sampling = NULL,
                         rcmdcheck_params = reversecheck_deafult_rcmd_params(),
                         minicran_type = "source",
                         minicran_filters = NULL,
                         ...) {
  
  pkg <- check_path_is_pkg_source(pkg)
  dependencies <- check_dependencies(dependencies)
  checkmate::assert_function(sampling, null.ok = TRUE)
  checkmate::assert_character(rcmdcheck_params, null.ok = TRUE)
  
  setup_reversecheck(reversecheck_dir)
  
  reversecheck_initialize(
    pkg = pkg,
    reversecheck_dir = reversecheck_dir,
    lib.loc = lib.loc,
    dependencies = dependencies,
    repos = repos,
    dependencies_repos = dependencies_repos,
    sampling = sampling,
    minicran_type = minicran_type,
    minicran_filters = minicran_filters,
    ...
  )
  
  reversecheck_run(
    pkg = pkg,
    reversecheck_dir = reversecheck_dir,
    lib.loc = lib.loc,
    n_childs = n_childs,
    repos = repos,
    rcmdcheck_params = rcmdcheck_params,
    ...
  )
  
  reversecheck_report()
  
  invisible(NULL)
}


reversecheck_deafult_rcmd_params <- function() {
  c()
}