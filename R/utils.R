#' @import cli
"_PACKAGE"

enum <- function(...) {
  x <- c(...)
  f <- factor(x, levels = x)
  names(f) <- levels(f)
  lapply(f, identity)
}

#' @export
Ops.factor <- function(e1, e2) {
  # nolint start, styler: off
  switch(.Generic,
    ">" = , ">=" = , "==" = , "<" = , "<= " =
      do.call(.Generic, list(as.numeric(e1), as.numeric(e2))),
    NextMethod()
  )
  # nolint end, styler: on
}

STATUS <- enum( # nolint
  "pending",
  "in progress",
  "done"
)

DEP <- enum( # nolint
  "Imports",
  "Depends",
  "LinkingTo",
  "Enhances",
  "Suggests"
)

DEP_STRONG <- unlist(DEP[1:3]) # nolint

base_pkgs <- function() {
  ip <- installed.packages()
  c("R", ip[!is.na(ip[, "Priority"]) & ip[, "Priority"] == "base", "Package"])
}

split_packages_names <- function(x) {
  if (is.na(x)) {
    x
  } else {
    vcapply(
      tools:::.split_dependencies(x),
      "[[",
      "name",
      USE.NAMES = FALSE
    )
  }
  # unname(
  #   gsub(
  #     "^\\s+|\\s+$", "",
  #     unlist(
  #       strsplit(gsub("\\s*\\(.*?\\)\\s*", "", x), ",\\s*")
  #     )
  #   )
  # )
}

replace_with_map <- function(x, value, replacement) {
  m <- match(x, value)
  x[which(!is.na(m))] <- replacement[m[!is.na(m)]]
  x
}

raw_based_hash <- function(x) {
  paste0(c("hash", as.character(charToRaw(x))), collapse = "")
}

check_path_is_pkg_source <- function(pkg) {
  checkmate::assert_string(pkg)
  checkmate::assert_directory_exists(pkg)
  checkmate::assert_file_exists(file.path(pkg, "DESCRIPTION"))
  normalizePath(pkg, mustWork = TRUE)
}

check_dependencies <- function(dependencies) {
  is_all <- isTRUE(dependencies)
  is_strong <- length(dependencies) == 1 && is.na(dependencies)

  dependencies <- if (dependencies == "all" || is_all) {
    as.character(unlist(DEP))
  } else if (dependencies == "strong" || is_strong) {
    as.character(unlist(DEP_STRONG))
  } else if (is.character(dependencies)) {
    valid_deps <- dependencies %in% unlist(DEP)
    if (!all(valid_deps)) {
      warning(
        "Passed dependencies names does not match standard R dependencies ",
        "names. The non-standard names has been removed."
      )
    }
    dependencies[valid_deps]
  } else {
    stop(
      "Dependencies has to be a TRUE/NA logical or character vector with ",
      "names of standard R dependencies."
    )
  }

  checkmate::assert_character(dependencies, min.len = 1)
  dependencies
}

get_package_name <- function(path) {
  if (file.exists(file.path(path, "DESCRIPTION"))) {
    read.dcf(file.path(path, "DESCRIPTION"))[, "Package"]
  } else {
    path
  }
}

dir_create <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
}

can_symlink <- function() {
  dir_create(tmp_dir <- file.path(tempdir(), "symlinktest"))
  suppressWarnings(file.symlink(tempfile(), tmp_dir))
}

symlink_or_copy <- function(from, to) {
  if (can_symlink()) {
    file.symlink(from = from, to = to)
  } else {
    file.copy(from = from, to = to, recursive = TRUE)
  }
}

is_package_done <- function(pkg, lib.loc) {
  path <- find.package(pkg, lib.loc = lib.loc, quiet = TRUE)
  length(path) > 0
}

`%||%` <- function(x, y) {
  if (!is.null(x)) {
    x
  } else {
    y
  }
}


#' @import cli
"_PACKAGE"

drlapply <- function(...) do.call(rbind, lapply(...))
drmapply <- function(...) do.call(rbind, mapply(..., USE.NAMES = FALSE, SIMPLIFY = FALSE))
uulist <- function(...) unique(as.character(unlist(...)))
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
vcapply <- function(...) vapply(..., FUN.VALUE = character(1L))
vlapply <- function(...) vapply(..., FUN.VALUE = logical(1L))
viapply <- function(...) vapply(..., FUN.VALUE = integer(1L))
vnapply <- function(...) vapply(..., FUN.VALUE = numeric(1L))

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

str_pad <- function(x, n) {
  x <- format(x)
  paste0(strrep(" ", n - nchar(x)), x)
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
