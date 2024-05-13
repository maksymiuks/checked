reversecheck_package <- function(name = NULL, alias = name, path = NULL, type = NULL, repos = NULL) {
  if (is.null(name) && !is.null(path) && !file.exists(file.path(path, "DESCRIPTION"))) {
    name <- get_package_name(path)
  }
  
  structure(
    list(
      alias = alias,
      name = name,
      path = path,
      type = if (!is.null(path)) "source" else getOption("pkgType"),
      repos = repos
    ),
    # add reversecheck suffix to the package to avoid ambiguity with devtools's
    # 'package' object
    class = "reversecheck_package"
  )

}
