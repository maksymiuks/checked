#' @export
report_sleep.reporter_ansi_tty <- function(reporter, plan, sleep = 0.1) {  # nolint
  Sys.sleep(sleep)
}

#' @export
report_initialize.reporter_ansi_tty <- function(  # nolint
    reporter,
    plan,
    envir = parent.frame()) {
  reporter$active <- integer()
  reporter$idx <- 0
  n_char_titles <- max(viapply(plan$queue, function(i) nchar(i$name)))

  # hide cursor when initializer enters, ensure its restored even if interrupted
  cli::ansi_hide_cursor()
  do.call(
    on.exit,
    list(quote(cli::ansi_show_cursor()), add = TRUE), 
    envir = envir
  )

  cat(
    strrep(" ", n_char_titles + 2),
    cli_table_row("S", "OK", "N", "W", "E", title = TRUE), "\n",
    sep = ""
  )

  cli::cli_progress_bar(
    type = "custom",
    format = "ETA {cli::pb_eta} ({cli::pb_current}/{cli::pb_total}) [{cli::pb_elapsed}]",
    format_done = "Finished in {cli::pb_elapsed}",
    total = length(plan$queue),
    clear = FALSE,
    .envir = reporter
  )
}

#' @export
report_status.reporter_ansi_tty <- function(reporter, plan, envir) { # nolint
  statuses <- plan$statuses()
  active <- which(statuses == 2)
  n_char_titles <- max(viapply(plan$queue, function(i) nchar(i$name)))

  # then update any older processes
  for (i in sort(unique(c(active, reporter$active)))) {
    width <- cli::console_width() - n_char_titles - 2
    status <- plan$queue[[i]]$process$cli_status_line(width = width)
    item_name <- plan$queue[[i]]$name
    is_new <- i > reporter$idx
    n_lines <- reporter$idx - i + 1L

    cat(
      if (!is_new) ansi_move_line_rel(n_lines),
      if (!is_new) ansi_line_erase(),
      " ", strrep(" ", n_char_titles - nchar(item_name)), item_name, " ",
      status,
      if (!is_new) ansi_move_line_rel(-n_lines) else "\n",
      sep = ""
    )

    # update last reported status flag, which may lag behind actual status
    reporter$idx <- max(reporter$idx, i)
    reporter$active <- active
  }

  cli::cli_progress_update(set = sum(statuses >= 3), .envir = reporter)
}

#' @export
report_finalize.reporter_ansi_tty <- function(reporter, plan) { # nolint
  report_status(reporter, plan) # report completions of final processes
  cli::cli_progress_done(.envir = reporter)
  cli::ansi_show_cursor()
}
