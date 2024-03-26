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
    stop("basic reporter not yet available")
    reporter("basic")
  }
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
