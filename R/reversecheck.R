#' @export
reversecheck <- function(
    ...,
    n_childs = 2,
    sampling = NULL,
    rcmdcheck_args = reversecheck_deafult_rcmdcheck_args(),
    minicran_type = .Platform$pkgType,
    minicran_filters = NULL) {
  checkmate::assert_function(sampling, null.ok = TRUE)
  checkmate::assert_character(rcmdcheck_args, null.ok = TRUE)

  reversecheck_initialize(
    ...,
    minicran_type = minicran_type,
    minicran_filters = minicran_filters,
  )

  reversecheck_run(
    ...,
    n_childs = n_childs,
    rcmdcheck_params = rcmdcheck_args,
    type = minicran_pkg_type(minicran_type),
  )

  #  reversecheck_report()

  invisible(NULL)
}


reversecheck_deafult_rcmdcheck_args <- function() {
  c()
}
