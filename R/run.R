run <- function(paths, ..., reporter = default_reporter()) {
  plan <- revcheck_plan(paths, n_parallel = 2)
  on.exit(plan$kill_all(), add = TRUE)

  report_initialize(reporter, plan)
  while (!plan$finished()) {
    while (plan$start_next()) {}
    report_status(reporter, plan)
    Sys.sleep(0.1)
  }
  report_finalize(reporter, plan)

  invisible(plan)
}
