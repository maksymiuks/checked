cli_table_row <- function(
  status, ok = "OK", notes = "N", warnings = "W", errors = "E", msg = "",
  title = FALSE
) {
  status <- trimws(as.character(status))
  status <- switch(status,
    "1" = , "2" = , "OK" = , "3" = , "NONE" = cli::col_green("✓"),
    "4" = , "NOTE" = cli::col_blue("!"),
    "5" = , "WARNING" = cli::col_magenta("?"),
    "6" = , "ERROR" = cli::col_yellow("⨯"),
    if (title) cli::col_none(cli::style_bold(status)) else status
  )

  ok <- str_pad(ok, n = 2)
  notes <- str_pad(notes, n = 2)
  warnings <- str_pad(warnings, n = 2)
  errors <- str_pad(errors, n = 2)

  if (title) {
    ok <- cli::col_none(cli::style_bold(ok))
    notes <- cli::col_none(cli::style_bold(notes))
    warnings <- cli::col_none(cli::style_bold(warnings))
    errors <- cli::col_none(cli::style_bold(errors))
  } else {
    ok <- cli::col_none(ok)
    notes <- cli::col_blue(notes)
    warnings <- cli::col_magenta(warnings)
    errors <- cli::col_yellow(errors)
  }

  fmt <- "│ {status} │ {ok} {notes} {warnings} {errors} │ {msg}"
  cli::format_inline(fmt)
}
