next_task_to_run <- function(g, output) {
  checks <- task_graph_which_check_satisfied(g)
  installs <- task_graph_which_install_satisfied(g)

  # Prioritize checks overs installs
  v <- igraph::V(g)[c(checks, installs)]
  head(v, 1L)
}

task_get_lib_loc <- function(g, node, output) {
  nhood <- task_graph_neighborhoods(g, node)[[1]]
  name <- names(node) %||% node
  nhood <- nhood[names(nhood) != .env$name]
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
    path_check_output(output, attributes$spec[[1]]$alias)
  } else if (attributes$custom) {
    path_custom_lib(output, attributes$spec[[1]]$alias)
  } else {
    path_lib(output)
  }
}

start_task <- function(task, g, ...) {
  UseMethod("start_task", task_graph_task_spec(g, task))
}

start_task.install_task_spec <- function(task, g, output, lib.loc, ...) {
  spec <- task_graph_task_spec(g, task)
  package <- if (is.null(spec$path)) spec$name else spec$path
  libpaths <- c(task_get_lib_loc(g, task, output), lib.loc)
  install_packages_process$new(
    package,
    lib = path_lib(output),
    libpaths = libpaths,
    repos = spec$repos,
    type = spec$type,
    INSTALL_opts = spec$INSTALL_opts,
    log = path_package_install_log(output, spec$alias),
    env = spec$env
  )
}

start_task.custom_install_task_spec <- function(task, g, output, lib.loc, ...) {
  spec <- task_graph_task_spec(g, task)
  package <- if (is.null(spec$path)) spec$name else spec$path
  libpaths <- c(task_get_lib_loc(g, task, output), lib.loc)
  install_packages_process$new(
    package,
    lib = path_custom_lib(output, spec$alias),
    libpaths = libpaths,
    repos = spec$repos,
    type = spec$type,
    INSTALL_opts = spec$INSTALL_opts,
    log = path_package_install_log(output, spec$alias),
    env = spec$env
  )
}

start_task.check_task_spec <- function(task, g, output, lib.loc, ...) {
  spec <- task_graph_task_spec(g, task)
  libpaths <- c(task_get_lib_loc(g, task, output), lib.loc)
  path <- if (!file.exists(spec$path)) {
    fetch_package_source(spec$path, path_sources(output))
  } else {
    spec$path
  }

  check_process$new(
    path = path,
    check_dir = path_check_output(output, spec$alias),
    libpath = libpaths,
    repos = spec$repos,
    args = spec$check_args,
    build_args = spec$build_args,
    env = spec$env
  )
}
