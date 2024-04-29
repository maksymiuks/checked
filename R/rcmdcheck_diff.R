ISSUES_TYPES <- c("notes", "warnings", "errors")

diff.rcmdcheck <- function(new, old, issues = ISSUES_TYPES, ...) {
  checkmate::assert_class(old, "rcmdcheck")
  issues <- match.arg(issues, ISSUES_TYPES, several.ok = TRUE)

  structure(
    lapply(issues, function(i) {
      new_i <- structure(
        new[[i]],
        names = get_issue_header(new[[i]])
      )
      old_i <- structure(
        old[[i]],
        names = get_issue_header(old[[i]])
      )

      matching_headers_idx <- names(new_i) %in% names(old_i)
      matching_messages_idx <- new_i %in% old_i

      new_issues <- structure(
        unname(new_i[!matching_headers_idx]),
        class = "issues"
      )
      new_potential_issues <- new_i[matching_headers_idx & !matching_messages_idx]
      new_potential_issues <- structure(
        list(
          new = unname(new_potential_issues),
          old = unname(old_i[names(new_potential_issues)])
        ),
        class = "potential_issues"
      )

      list("issues" = new_issues, "potential_issues" = new_potential_issues)
    }),
    names = issues,
    class = "rcmdcheck_diff"
  )
}

print.rcmdcheck_diff <- function(x, ...) {
  cat("R CMD check diff \n\n")
  for (i in ISSUES_TYPES) {
    status <- if (length(x[[i]]$issues) > 0) {
      "NEW ISSUES"
    } else if (length(x[[i]]$potential_issues$new) > 0) {
      "NEW POTENTIAL ISSUES"
    } else {
      "OK"
    }

    cat(sprintf("%s: %s", i, status), "\n")
    print(x[[i]]$issues)
    print(x[[i]]$potential_issues)
  }
}

print.issues <- function(x, ...) {
  cat(x, sep = "\n\n")
}

print.potential_issues <- function(x, ...) {
  for (i in seq_along(x$new)) {
    print(cli::diff_chr(
      strsplit(x$old[i], "\n")[[1]],
      strsplit(x$new[i], "\n")[[1]]
    ))
    cat("\n\n\n")
  }
  invisible(x)
}

diff_collection <- function(new, old, issues) {
  structure(
    mapply(diff, new, old),
    class = c("diff_collection", "list"),
    names = names(new)
  )
}

aggregate.diff_collection <- function(x, ...) {

}

get_issue_header <- function(x) {
  unname(vapply(x, function(y) {
    strsplit(y, "...", fixed = TRUE)[[1]][1]
  }, FUN.VALUE = character(1)))
}

rcmdcheck_to_json <- function(rcheck, file = NULL) {
  checkmate::assert_class(rcheck, "rcmdcheck")

  json <- jsonlite::toJSON(
    unclass(rcheck),
    auto_unbox = TRUE,
    pretty = TRUE,
    force = TRUE # This is crucial to skip any environments in the rcheck object
  )

  if (!is.null(path)) {
    jsonlite::write_json(json, path)
  }

  json
}


rcmdcheck_from_json <- function(file) {
  checkmate::assert_file_exists(file, access = "r")

  structure(
    jsonlite::fromJSON(file),
    class = "rcmdcheck"
  )
}
