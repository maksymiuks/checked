#' @export
reversecheck <- function(pkg = "./",
                         reversecheck_dir = file.path(pkg, "reversecheck"),
                         lib.loc = .libPaths(),
                         dependencies = TRUE,
                         n_childs = 1L,
                         repos = getOption("repos"),
                         dependencies_repos = repos,
                         filters = NULL,
                         sampling = NULL,
                         rcmdcheck_params = reversecheck_deafult_rcmd_params(),
                         ...) {
  pkg <- check_path_is_pkg_source(pkg)
  dependencies <- check_dependencies(dependencies)
  checkmate::assert_function(sampling, null.ok = TRUE)
  checkmate::assert_character(rcmdcheck_params, null.ok = TRUE)


  invisible(NULL)
}


reversecheck_deafult_rcmdcheck_args <- function() {
  c()
}
