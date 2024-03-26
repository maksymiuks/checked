cli_table_row <- function(status, ok, notes, warnings, errors, msg = "") {
  fmt <- "│ {status} │ {ok} {notes} {warnings} {errors} │ {msg}"
  cli::format_inline(fmt)
}

#' @export
run <- function(packages) {
  cli::ansi_with_hidden_cursor({
    max_package_name_len <- max(nchar(names(packages)))

    cat(
      " ", strrep(" ", max_package_name_len), " ",
      cli_table_row("S", "OK", " N", " W", " E"),
      "\n",
      sep = ""
    )

    # repeatedly update cli output for any active packages
    alive <- 0
    n_process <- 2
    n_started <- 0

    # print(environment())
    cli::cli_progress_bar(
      type = "custom",
      format = "ETA {cli::pb_eta} ({cli::pb_current}/{cli::pb_total}) [{cli::pb_elapsed}]",
      total = length(packages),
    )

    repeat {
      alive <- alive + viapply(packages, function(i) inherits(i, "process") && i$is_alive())
      n_alive <- sum(alive > 1)

      if (n_alive < n_process) {
        remaining <- !vlapply(packages, inherits, "process")
        for (i in utils::head(which(remaining), n_process - n_alive)) {
          package <- names(packages[i])
          packages[[i]] <- revcheck_process$new(
            package,
            args = "--no-manual",
            build_args = "--no-manual"
          )

          cat(
            " ",
            strrep(" ", max_package_name_len - nchar(names(packages[i]))),
            names(packages[i]),
            " ",
            packages[[i]]$cli_status_line(),
            "\n",
            sep = ""
          )

          n_started <- n_started + 1
          alive[[i]] <- 2
        }
      }

      statuses <- vcapply(packages, function(i) {
        if (!inherits(i, "revcheck_process")) return("")
        i$cli_status_line(width = cli::console_width() - max_package_name_len - 2)
      })

      for (i in which(alive > 0)) {
        n <- n_started - i + 1
        cat(
          ansi_move_line_rel(n), ansi_line_erase(),
          " ",
          strrep(" ", max_package_name_len - nchar(names(packages[i]))),
          names(packages[i]),
          " ",
          statuses[[i]],
          ansi_move_line_rel(-n),
          sep = ""
        )
      }

      alive <- pmax(alive - 1, 0)
      cli::cli_progress_update(set = n_started - sum(alive > 0))
      if (!any(alive > 0)) break
      Sys.sleep(0.1)
    }
  })
}
