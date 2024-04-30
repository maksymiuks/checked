#' @export
format_status_line_ansi <- function(process, ...) {
  UseMethod("format_status_line_ansi")
}

#' @export
format_status_line_ansi.default <- function(process, ...) {
  format(process, ...)
}

#' @export
report_sleep.reporter_ansi_tty <- function(reporter, design, sleep = 0.1) { # nolint
  Sys.sleep(sleep)
}

#' @export
report_initialize.reporter_ansi_tty <- function(
    reporter,
    design,
    envir = parent.frame()) {
  # named factor vector, names as task aliases and value of last reported status
  reporter$status <- STATUS$pending[c()]
  n_char_titles <- max(nchar(design$checks$alias))

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
    extra = list(message = ""),
    format = "ETA {cli::pb_eta} ({cli::pb_current}/{cli::pb_total}) [{cli::pb_elapsed}] {cli::pb_extra$message}", # nolint
    format_done = "Finished in {cli::pb_elapsed}",
    total = nrow(design$checks),
    clear = FALSE,
    auto_terminate = FALSE,
    .envir = reporter,
  )
}

#' @export
report_status.reporter_ansi_tty <- function(reporter, design, envir) { # nolint
  n_char_titles <- max(nchar(design$checks$alias))

  # add newly started task status
  new_idx <- which(design$checks$process > STATUS$pending)
  new_idx <- new_idx[!design$checks$alias[new_idx] %in% names(reporter$status)]
  if (length(new_idx) > 0) {
    # always start by reporting in progress, even if finished before reporting
    new <- rep_len(STATUS$`in progress`, length(new_idx))
    names(new) <- design$checks$alias[new_idx]
    reporter$status <- c(reporter$status, new)
  }

  # for each not-yet finished task, report status
  for (idx in which(reporter$status < STATUS$done)) {
    alias <- names(reporter$status)[[idx]]
    process_idx <- which(design$checks$alias == alias)

    is_new <- process_idx %in% new_idx
    n_lines <- length(reporter$status) - idx + 1L
    width <- cli::console_width() - n_char_titles - 2
    task_name <- design$checks$alias[[process_idx]]
    status <- format_status_line_ansi(
      design$checks$process[[process_idx]],
      width = width
    )

    cat(
      if (!is_new) ansi_move_line_rel(n_lines),
      if (!is_new) ansi_line_erase(),
      " ", strrep(" ", n_char_titles - nchar(task_name)), task_name, " ",
      status,
      if (!is_new) ansi_move_line_rel(-n_lines) else "\n",
      sep = ""
    )

    # update last reported status flag, which may lag behind actual status
    reporter$status[[idx]] <- get_process_status(design$checks$process[[idx]])
  }

  is_inst <- vlapply(
    design$active_processes(),
    inherits,
    "install_package_process"
  )
  inst_pkgs <- names(design$active_processes()[is_inst])
  if (length(inst_pkgs)) {
    inst_msg <- paste0("installing ", paste0(inst_pkgs, collapse = ", "))
  } else {
    inst_msg <- ""
  }

  n_finished <- viapply(design$checks$process, get_process_status) >= 3
  cli::cli_progress_update(
    set = sum(n_finished),
    extra = list(message = inst_msg),
    .envir = reporter
  )
}

#' @export
report_finalize.reporter_ansi_tty <- function(reporter, design) { # nolint
  report_status(reporter, design) # report completions of final processes
  cli::cli_progress_done(.envir = reporter)
  cli::ansi_show_cursor()
}
