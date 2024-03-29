#' Regular Expression for Parsing R CMD check checks
# nolint start
RE_CHECK <- paste0(  
  "(?<=^|\n)",       # starts on a new line (or start of string)
  "\\* checking ",   # literal "* checking "
  "(?<check>.*?)",   # capture any check content as "check"
  " \\.\\.\\.",      # until literal "..."
  "(?:",             # ignore additional check details:
    "[\\s\\*]{2,}",  #   any content starting with two or more of spaces or "*"
    ".*?(?:\n|$)",   #   until a newline (or end of string)
  ")*",              #   repeating until
  "(?<status>.*?)",  # capturing a status as "status"
  "(?=\n|$)"         # terminated by a new line (or end of string)
)                    
# nolint end

#' @importFrom R6 R6Class
#' @importFrom rcmdcheck rcmdcheck_process
revcheck_process <- R6::R6Class(
  "revcheck_process",
  inherit = rcmdcheck::rcmdcheck_process,
  active = list(
    checks = function() {
      self$poll_output()
      private$parsed_checks
    }
  ),
  public = list(
    initialize = function(...) {
      args <- as.list(match.call(rcmdcheck::rcmdcheck, expand.dots = TRUE)[-1])
      args <- lapply(args, eval, envir = parent.frame())

      private$args <- args
      private$throttle <- throttle()
      private$spinners <- list(
        check = silent_spinner("circleHalves"),
        starting = silent_spinner(list(frames = c("⠴", "⠦", "⠖", "⠲")))
      )

      super$initialize(...)
    },

    get_time_last_check_start = function() {
      private$time_last_check_start
    },

    get_duration = function() {
      (self$get_time_finish() %||% Sys.time()) - self$get_start_time()
    },

    get_time_finish = function() {
      private$time_finish
    },

    get_status_counts = function() {
      self$poll_output()
      table(private$parsed_checks)
    },

    poll_output = function() {
      if (private$throttle()) return()
      if (!self$is_alive()) {
        private$time_last_check_start <- NULL
        private$time_finish <- Sys.time()
      }

      out <- paste0(private$parsed_partial_check_output, super$read_output())
      captures <- checks_capture(out)
      checks <- checks_simplify(captures)

      if (length(checks) > 0) {
        private$time_last_check_start <- Sys.time()
        unknown <- !checks %in% levels(private$parsed_checks)
        checks[unknown] <- "NONE"
        private$parsed_checks[names(checks)] <- checks
      }

      if (length(private$parsed_checks) == 0) {
        # no checks were parsed
        private$parsed_partial_check_output <- out
      } else if (identical(tail(checks, 1), "")) {
        # the most recent output's check is still running
        n <- nrow(captures)
        private$parsed_partial_check_output  <- substring(out, captures[1, n])
      } else {
        # the final check was fully parsed
        private$parsed_partial_check_output  <- ""
      }
    },

    cli_status_line = function(width = cli::console_width()) {
      self$poll_output()
      check_codes <- as.numeric(private$parsed_checks)

      # runtime of process
      process_time <- paste0(format_time(self$get_duration()), " ")

      # runtime of current check (only displayed if >30s)
      check_time <- Sys.time() - self$get_time_last_check_start()
      if (length(check_time) == 0 || check_time < difftime(30, 0)) {
        check_time <- ""
      } else {
        check_time <- cli::col_grey("(", format_time(check_time), ") ")
      }

      msg <- ""
      status <- max(check_codes, -1)
      if (length(private$parsed_checks) == 0) {
        # have not hit checks yet
        msg <- "starting ..."
        status <- private$spinners[["starting"]]$spin()
      } else if (self$is_alive()) {
        # processing checks
        msg <- paste("checking", names(tail(private$parsed_checks, 1)), "...")
        status <- private$spinners[["check"]]$spin()
        process_time <- cli::col_cyan(process_time)
      } else {
        # done
        process_time <- cli::col_grey(process_time)
      }

      msg <- cli::format_inline("{process_time}{check_time}{msg}")
      counts <- self$get_status_counts() 
      out <- cli_table_row(
        status = status,
        ok = counts[["NONE"]] + counts[["OK"]],
        notes = counts[["NOTE"]],
        warnings = counts[["WARNING"]],
        errors = counts[["ERROR"]],
        msg
      )

      cli::ansi_substring(out, 1, width)
    }
  ),
  private = list(
    args = list(),
    time_last_check_start = NULL,
    time_finish = NULL,
    parsed_checks = factor(levels = c("", "NONE", "OK", "NOTE", "WARNING", "ERROR")),
    parsed_partial_check_output = "",
    throttle = NULL,
    spinners = NULL
  )
)

#' Generate A Rate Limiting Throttle Function
#'
#' @param interval An interval (in seconds) that is the minimum interval
#'   before [throttle] will return `TRUE`.
#' @return A throttling function with the provided interval. When called,
#'   returns a logical value indicating whether the throttle interval has
#'   passed (TRUE if the interval has not yet passed).
#'
#' @examples
#' my_throttle <- throttle(0.5)
#'
#' Sys.sleep(0.1)
#' my_throttle()
#'
#' Sys.sleep(0.5)
#' my_throttle()
#'
throttle <- function(interval = 0.2) {
  e <- environment()
  e$since <- Sys.time()
  function(since = e$since, interval = e$interval) {
    if (Sys.time() - since < interval) {
      TRUE
    } else {
      e$since <- Sys.time()
      FALSE
    }
  }
}

#' Parse R CMD checks from a partial check output string
#'
#' @param x A string, compsoed of any subsection of R CMD check console output
#' @return A matrix of matches and capture groups "check" and "status"
#'   ("OK", "NONE", "NOTE", "WARNING" or "ERROR").
#'
#' @examples
#' check_output <- "
#' * checking check one ... OK
#' * checking check two ... NOTE
#' * checking tests ...
#'   Running test_abc.R
#'   Running test_xyz.R
#'  NONE
#' * checking check three ... WARNING
#' * ch
#' "
#'
#' checks_capture(check_output)
#'
checks_capture <- function(x) {
  m <- gregexec(RE_CHECK, x, perl = TRUE)[[1]]

  if (!is.matrix(m)) {
    captures <- c("check", "status")
    return(t(matrix(character(0L), nrow = 3, dimnames = list(c("", captures)))))
  }

  l <- attr(m, "match.length")
  t(matrix(
    substring(x, m, m + l - 1),
    nrow = nrow(m),
    ncol = ncol(m),
    dimnames = dimnames(m)
  ))
}

#' Simplify Captures into Vector
#'
#' @param x Matrix of regex captures as produced by [checks_capture].
#' @return A vector of check status, with names indicating the check
#'
checks_simplify <- function(x) {
  checks <- trimws(x[, "status"])
  names(checks) <- trimws(x[, "check"])
  checks
}
