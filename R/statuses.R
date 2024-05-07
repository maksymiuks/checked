#' @export
get_process_status <- function(x) {
  UseMethod("get_process_status")
}

#' @export
get_process_status.factor <- function(x) {
  x
}

#' @export
get_process_status.NULL <- function(x) {
  STATUS$pending
}

#' @export
get_process_status.revdep_process <- function(x) {
  if (x$is_alive()) STATUS$`in progress` else STATUS$done
}
