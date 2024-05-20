next_task_to_run <- function(g, output) {
  checks <- task_graph_which_check_satisfied(g)
  installs <- task_graph_which_satisfied_strong(g)
  
  # Prioritize checks overs installs
  v <- igraph::V(g)[c(checks, installs)]
  next_task <- head(v, 1L)
  if (length(next_task) > 0) {
    attrs <- igraph::vertex.attributes(g, next_task)
    
    structure(
      list(
        v = next_task,
        install_lib = task_get_install_lib(g, next_task, output),
        lib.loc = task_get_lib_loc(g, next_task, output),
        package = attrs$package
      ),
      class = attrs$type
    )
  }
}

task_get_lib_loc <- function(g, node, output) {
  nhood <- task_graph_neighborhoods(g, node)[[1]]
  name <- names(node) %||% node
  nhood <- nhood[names(nhood) != name]
  # Custom packages are possible only for the check type nodes which are
  # always terminal. Therefore if we sort nhood making custom packages appear
  # first, their lib will always be prioritized
  attributes <- igraph::vertex.attributes(g, index = nhood)
  
  paths <- vcapply(nhood, function(v) {
    task_get_install_lib(g, v, output)
  })
  
  unique(paths[order(attributes$custom, decreasing = TRUE)])
}

task_get_install_lib <- function(g, node, output) {
  attributes <- igraph::vertex.attributes(g, index = node)
  if (attributes$type == "check") {
    path_check_output(output, attributes$package[[1]]$alias)
  } else if (attributes$custom) {
    path_custom_lib(output, attributes$package[[1]]$alias)
  } else {
    path_lib(output)
  }
}

start_task <- function(task, ...) {
  UseMethod("start_task")
}

start_task.install <- function(task, output, lib.loc, ...) {
  pkg <- task$package[[1]]
  package <- if (is.null(pkg$path)) pkg$name else pkg$path
  libpaths <- c(task$lib.loc, lib.loc)
  install_packages_process$new(
    package,
    lib = task$install_lib,
    libpaths = libpaths,
    repos = pkg$repos,
    type = pkg$type,
    log = path_package_install_log(output, pkg$alias)
  )
}

start_task.check <- function(task, output, lib.loc, ...) {
  process <- mock_process$new(runif(1, 15, 25), class = "revdep_process") # fake process, lasts ~20s
  # lib.loc = task$lib.loc
  # this process needs to:
  #   - set up libpaths
  #   - set up appropriate, restricted library, linking to cache
  #   - run r cmd check
}
