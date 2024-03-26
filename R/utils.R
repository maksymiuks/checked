check_input_pkg <- function(pkg) {
  checkmate::assert_string(pkg)
  checkmate::assert_directory_exists(pkg)
  checkmate::assert_file_exists(file.path(pkg, "DESCRIPTION"))
  
  normalizePath(pkg, mustWork = TRUE)
}

check_input_dependencies <- function(dependencies) {
  
  
  dependencies <- if (isTRUE(dependencies)) {
    c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  } else if (length(dependencies) == 1 && is.na(dependencies)) {
    c("Depends", "Imports", "LinkingTo")
  } else if (is.character(dependencies)) {
    dependencies
  } else {
    stop("Dependencies has to be a TRUE/NA logical or character vector with ",
         "names of standard R dependencies.")
  }
  
  matched_deps <- dependencies[dependencies %in% DEPENDENCIES]
  
  dependencies <- if (!identical(matched_deps, dependencies)) {
    warning("Passed dependencies names does not match standard R dependencies names. ",
            "The non-standard names has been removed")
    matched_deps
  } else {
    dependencies
  }
  
  checkmate::assert_character(dependencies, min.len = 1)
  
  dependencies
}

get_package_name <- function(path) {
  read.dcf(file.path(path, "DESCRIPTION"))[, "Package"]
}

dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

#' @import cli
"_PACKAGE"

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
vcapply <- function(...) vapply(..., FUN.VALUE = character(1L))
vlapply <- function(...) vapply(..., FUN.VALUE = logical(1L))
viapply <- function(...) vapply(..., FUN.VALUE = integer(1L))

#' Reuse or Create A Null File Connection
devnull <- function() {
  cons <- showConnections(all = TRUE)[, "description"]
  if (length(w <- which(nullfile() == cons))) {
    getConnection(names(cons)[[w[[1]]]])
  } else {
    file(nullfile())
  }
}

format_time <- function(x) {
  n <- unclass(x)
  sprintf("%.01f%s", n, substring(attr(n, "units"), 1, 1))
}

#' Create a 'cli' Spinner With Suppressed Output
#'
#' 'cli' will implicitly push spinner output to various output streams,
#' affecting the terminal cursor position. To allow for a terminal interface
#' that has spinners above the last line, this function suppresses the output
#' and simply returns its frame contents.
#'
#' @param ... passed to [cli::make_spinner]
#' @param stream passed to [cli::make_spinner], defaults to a null file device
#' @return A interface similar to a 'cli' spinner, but with suppressed output
#'
#' @importFrom cli make_spinner
silent_spinner <- function(..., stream = devnull()) {
  spinner <- cli::make_spinner(..., stream = stream)

  spin <- function(...) {
    spinner$spin(...)
    with(environment(spinner$spin), c_spinner$frames[[c_state]])
  }

  list(spin = spin, final = spinner$final)
}
