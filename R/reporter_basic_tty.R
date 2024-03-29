#' @export
report_initialize.reporter_basic_tty <- function(  # nolint
    reporter,
    plan,
    envir = parent.frame()) {
  reporter$time_start <- Sys.time()
  reporter$statuses <- new.env(parent = emptyenv())
  cli::cli_text("<", packageName(), "> Checks")
}

#' @export
report_status.reporter_basic_tty <- function(reporter, plan) { # nolint
  cli_theme()
  statuses <- plan$statuses()
  for (i in seq_along(plan$queue)) {
    # skip if queued, but not started
    if (statuses[[i]] <= 1) next

    # report stating of new checks
    plan$queue[[i]]$process$poll_output()
    name <- plan$queue[[i]]$name
    if (!identical(statuses[[i]], reporter$statuses[[name]])) {
      status <- switch(statuses[[i]],  # nolint
        "1" = "queued",
        "2" = cli::cli_fmt(cli::cli_text("started")),
        "3" = {
          dur <- plan$queue[[i]]$process$get_duration()  # nolint
          ewn <- c("ERROR", "WARNING", "NOTE")
          ewn <- plan$queue[[i]]$process$get_status_counts()[ewn]
          cli::cli_fmt(cli::cli_text(
            "finished",
            if (sum(ewn) > 0) " with ",
            paste(collapse = ", ", c(
              if (ewn[[1]] > 0) cli::format_inline("{.err {ewn[[1]]} ERROR{?/S}}"),
              if (ewn[[2]] > 0) cli::format_inline("{.warn {ewn[[2]]} WARNING{?/S}}"),
              if (ewn[[3]] > 0) cli::format_inline("{.note {ewn[[3]]} NOTE{?/S}}")
            )),
            cli::format_inline(" {.time_taken ({format_time(dur)})}")
          ))
        }
      )
      cli::cli_text(cli::col_cyan("[check] "), "{.pkg {name}} {status}")
      reporter$statuses[[name]] <- statuses[[i]]
    }
  }
}

#' @export
report_finalize.reporter_basic_tty <- function(reporter, plan) { # nolint
  cli_theme()
  report_status(reporter, plan) # report completions of final processes
  time <- format_time(Sys.time() - reporter$time_start)
  cli::cli_text("Finished in {.time_taken {time}}")
}
