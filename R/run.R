#' Run Reverse-Dependency Checks
#'
#' @param A reverse-dependency plan, or an object coercible into a
#'   plan.
#' @param n_parallel The number of simultaneous child processes to use.
#' @param ... Additional arguments unused
#' @param reporter A reporter to provide progress updates. Will default to the
#'   most expressive command-line reporter given your terminal capabilities.
#'
#' @export
run <- function(plan, n_parallel = 2, ..., reporter = default_reporter()) {
  UseMethod("run")
}

#' @export
run.character <- function(plan, ..., reporter) {
  plan <- revcheck_plan(plan, ...)
  NextMethod()
}

#' @export
run.plan <- function(plan, ..., reporter = default_reporter()) {
  on.exit(plan$kill_all(), add = TRUE)

  report_initialize(reporter, plan)
  while (!plan$finished()) {
    while (plan$start_next()) {}
    report_status(reporter, plan)
    report_sleep(reporter, plan)
  }
  report_finalize(reporter, plan)

  invisible(plan)
}
