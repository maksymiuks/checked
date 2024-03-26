check_pkg <- function(pkg) {
  checkmate::assert_string(pkg)
  checkmate::assert_directory_exists(pkg)
  checkmate::assert_file_exists(file.path(pkg, "DESCRIPTION"))
  
  normalizePath(pkg, mustWork = TRUE)
}

check_dependencies <- function(dependencies) {
  
  
  dependencies <- if (isTRUE(dependencies)) {
    c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  } else if (length(dependencies) == 1 && is.na(dependencies)) {
    c("Depends", "Imports", "LinkingTo")
  } else if (is.character(dependencies)) {
    dependencies
  } else {
    stop("Dependencies has to be a TRUE/NA logical or character vector with "
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
