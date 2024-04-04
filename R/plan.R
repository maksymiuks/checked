revcheck_plan <- function(paths, n_parallel = 4) {
  self <- structure(new.env(parent = baseenv()), class = "plan")
  process_started <- function(item) inherits(item$process, "process")
  process_finished <- function(item) process_started(item) && !item$process$is_alive()

  # initialize queue from self inputs
  self$queue <- lapply(paths, function(path) {
    as.environment(list(
      name = desc::desc(path)$get("Package")[[1]],
      args = list(
        path = path,
        args = "--no-manual",
        build_args = "--no-manual"
      ),
      process = NULL # placeholder for callr subprocess handle
    ))
  })

  self$start_next <- function(n = n_parallel) {
    idx_last_finished <- Position(process_finished, self$queue, right = TRUE, nomatch = 0)
    idx_next <- Position(Negate(process_started), self$queue, nomatch = NULL)
    if (is.null(idx_next)) {
      return(FALSE)
    } # no more to process
    if (idx_next - idx_last_finished > n) {
      return(FALSE)
    } # saturating n_parallel
    item <- self$queue[[idx_next]]
    item$process <- do.call(revcheck_process$new, item$args)
    TRUE
  }

  self$status <- function(i) {
    item <- self$queue[[i]]
    1 + process_started(item) + process_finished(item)
  }

  self$statuses <- function() {
    vnapply(seq_along(self$queue), self$status)
  }

  self$finished <- function() {
    min(self$statuses(), 3) == 3
  }

  self$kill_all <- function() {
    for (item in self$queue) {
      if (process_started(item)) item$process$kill()
    }
  }

  self
}
