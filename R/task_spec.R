task_spec <- function(name = NULL, alias = name, path = NULL, repos = NULL, env = NULL) {
  structure(
    list(
      name = name,
      alias = alias,
      path = path,
      repos = repos,
      env = env
    ),
    class = "task_spec"
  )
}

list_of_task_spec <- function(x, ...) {
  structure(x, class = c("list_of_task_spec", "list"))
}

#' @export
print.task_spec <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
format.task_spec <- function(x, ...) {
  paste0("<task ", x$alias, ">")
}

#' @export
format.list_of_task_spec <- function(x, ...) {
  vcapply(x, format)
}

install_task_spec <- function(type = getOption("pkgType"), INSTALL_opts = NULL, ...) {
  task_spec <- task_spec(...)
  install_spec <- list(
    type = type,
    INSTALL_opts = INSTALL_opts
  )
  structure(
    c(install_spec, task_spec),
    class = c("install_task_spec", class(task_spec))
  )
}

custom_install_task_spec <- function(...) {
  task_spec <- install_task_spec(...)

  class(task_spec) <- c("custom_install_task_spec", class(task_spec))
  task_spec
}

check_task_spec <- function(check_args = NULL, build_args = NULL, ...) {
  task_spec <- task_spec(...)
  check_spec <- list(
    check_args = check_args,
    build_args = build_args
  )

  structure(
    c(check_spec, task_spec),
    class = c("check_task_spec", class(task_spec))
  )
}

revdep_check_task_spec <- function(revdep, ...) {
  task_spec <- check_task_spec(...)
  task_spec["revdep"] <- list(revdep)
  class(task_spec) <- c("revdep_check_task_spec", class(task_spec))

  task_spec
}
