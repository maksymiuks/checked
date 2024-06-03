#' Run Reverse-Dependency Checks
#'
#' @param A reverse-dependency plan, or an object coercible into a
#'   plan.
#' @param n The number of simultaneous child processes to use.
#' @param ... Additional arguments unused
#' @param reporter A reporter to provide progress updates. Will default to the
#'   most expressive command-line reporter given your terminal capabilities.
#'
#' @export
run <- function(design, ...) {
  UseMethod("run")
}

#' @export
run.character <- function(design, ..., reporter = default_reporter()) {
  run(new_rev_dep_check_design(design, ...), reporter = reporter)
}

#' @export
run.check_design <- function(design, ..., reporter = default_reporter()) {
  on.exit(add = TRUE, {
    design$terminate()
    report_finalize(reporter, design)
  })

  report_initialize(reporter, design)
  while (design$step()) {
    report_status(reporter, design)
    report_sleep(reporter, design)
  }

  invisible(design)
}
