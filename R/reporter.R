#' Initialize a Reporter Object
#'
#' Reporters are primarily used as a data-less class for dispatch. They are
#' initialized with an empty environment, which can be used to store
#' arbitrary state during reporting.
#'
#' @noRd
#' @keywords internal
reporter <- function(type) {
  type <- paste0("reporter_", type)
  structure(new.env(parent = baseenv()), class = c(type, "reporter"))
}

default_reporter <- function() {
  if (cli::is_ansi_tty()) {
    reporter("ansi_tty")
  } else if (cli::is_dynamic_tty()) {
    stop("dynamic tty reporter not yet available")
    reporter("dynamic_tty")
  } else {
    reporter("basic_tty")
  }
}

#' Provide a default sleep period between updates
report_sleep <- function(reporter, plan, sleep) {
  UseMethod("report_sleep")
}

#' @export
report_sleep.default <- function(reporter, plan, sleep = 1) {
  Sys.sleep(sleep)
}

report_initialize <- function(reporter, plan, envir = parent.frame()) {
  UseMethod("report_initialize")
}

report_status <- function(reporter, plan, envir = parent.frame()) {
  UseMethod("report_status")
}

report_finalize <- function(reporter, plan, envir = parent.frame()) {
  UseMethod("report_finalize")
}
