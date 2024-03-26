reversecheck <- function(pkg = "./",
                         reversecheck_dir = "./reversecheck",
                         dependencies_dir = file.path(tempdir(), "reversecheck"),
                         dependencies = TRUE,
                         repos = getOption("repos"),
                         dependencies_repos = repos,
                         timeout = as.difftime(60, units = "mins"),
                         sampling = NULL,
                         rcmdcheck_params = reversecheck_deafult_rcmd_params(),
                         pre_clear = FALSE
                         ...) {
  
  pkg <- check_pkg(pkg)
  dependencies <- check_dependencies(dependencies)
  checkmate::assert_class(timeout, "difftime")
  checkmate::assert_function(sampling, null.ok = TRUE)
  checkmate::assert_character(rcmdcheck_params, null.ok = TRUE)
  
  if (pre_clear) {
    unlink(get_reversecheck_directory(), recursive = TRUE, force = TRUE)
  }
  
  
  setup_reversecheck(reversecheck_dir, dependencies_dir)
  
  state <- reversecheck_initialize(
    pkg = pkg,
    reversecheck_dir = reversecheck_dir,
    dependencies = dependencies,
    repos = repos,
    sampling = sampling,
    ...
  )
  state <- reversecheck_check(
    pkg = pkg,
    reversecheck_dir = reversecheck_dir,
    timeout = timeout,
    rcmdcheck_params = rcmdcheck_params,
    ...
  )
  state <- reversecheck_report(state)
  
  invisible(NULL)
}

test <- function(...) {
  cl <- match.call()
  print(paste0(...))
  cl
}
