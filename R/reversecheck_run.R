reversecheck_run <- function(pkg, reversecheck_dir, lib.loc, n_childs, repos, 
                             dependencies_repos, filters, rcmdcheck_params, ...) {
  
  revdeps <- get_revdeps_from_dir(reversecheck_dir)
  G <- dep_graph_create(
    revdeps[!revdeps$status %in% c("IN_PROGRESS", "DONE"), "package"],
    availPkgs = utils::available.packages(repos = dependencies_repos, filters = filters)
  )
  processes <- list()
  inf_loop_counter <- 0
  
  while (!scheduler_loop_finished(revdeps, processes)) {
    running_processes <- vlapply(processes, function(p) p$is_alive())
    # If we have max number of processes started and none of them finished
    # wait a bit and run again
    if (length(processes) == n_childs) {
      if (all(running_processes)) {
        Sys.sleep(1)
        next()
      } else {
        processes[[which(!running_processes)[1]]] <- NULL
      }
    }
    
    # Update revdeps
    revdeps <- get_revdeps_from_dir(reversecheck_dir)
    todo <- revdeps[revdeps$status == "TODO", ]
    ready <- revdeps[revdeps$status %in% c("READY", "IN_PROGRESS_OLD"), ]
    
    G <- dep_graph_update_installed(G, reversecheck_lib_loc(lib.loc, reversecheck_dir))
    G <- dep_graph_sort(G)
    next_packages_install <- dep_graph_which_satisfied_strong(G)
    next_packages_check <- dep_graph_which_root_satisfied(G)

    
    processes <- if (NROW(ready) > 0) {
      p <- ready[1, ]
      process <- if(p$status == "READY") {
        set_revdep_status(reversecheck_dir, p$package, "IN_PROGRESS_OLD")
        revcheck_process(p$package, reversecheck_dir, "old", lib.loc = reversecheck_check_lib_loc(p$package, lib.loc, reversecheck_dir, "old"))
      } else {
        set_revdep_status(reversecheck_dir, p$package, "IN_PROGRESS")
        revcheck_process(p$package, reversecheck_dir, "new", lib.loc = reversecheck_check_lib_loc(p$package, lib.loc, reversecheck_dir, "new"))
      }
      append(processes, process)
    } else if (any(todo$package %in% next_packages_check)) {
      
      p <- todo[which(todo$package %in% next_packages_check)[1], ]
      # revdeps that are not dependencies for any other dependencies can
      # be immediately marked as installed 
      if (!dep_graph_is_dependency(G, p$package)) {
        G <- dep_graph_set_package_status(G, p$package, "installed")
      }
      
      set_revdep_status(reversecheck_dir, p$package, "PREPARING")
      process <- prepare_filesystem_process$new(
        p$package,
        get_package_name(pkg),
        reversecheck_dir,
        repos = repos,
        lib.loc = reversecheck_lib_loc(lib.loc, reversecheck_dir)
      )
      append(processes, process) 
    } else if (length(next_packages_install > 0)) {
      p <- next_packages_install[1]
      G <- dep_graph_set_package_status(G, p, "installing")

      process <- install_packages_process$new(
        pkgs = p, 
        lib = path_lib(reversecheck_dir, "cache"), 
        keep_outputs = file.path(path_logs(reversecheck_dir, "cache"), make.names(p)),
        repos = dependencies_repos,
        filters = filters,
        lib.loc = reversecheck_lib_loc(lib.loc, reversecheck_dir), 
        logs_path = file.path(path_logs(reversecheck_dir, "cache"), make.names(p), "subprocess.log")
      )
      append(processes, process) 
    } else {
      if (infinite_loop_test(revdeps, processes)) {
        inf_loop_counter <- inf_loop_counter + 1
      } else {
        inf_loop_counter <- 0
      }
    
      
      if (inf_loop_counter == 30) {
        stop("Infinite loop detected.")
      }
      processes
    }
  }
}


scheduler_loop_finished <- function(revdeps, processes) {
  # To stop the loop we want all revdeps to have "DONE" status
  # and all processes to finish
  done <- all(revdeps$status == "DONE")
  finished <- all(vlapply(processes, function(p) !p$is_alive()))
  
  done && finished
}

infinite_loop_test <- function(revdeps, processes) {
  any_not_done <- any(revdeps$status != "DONE")
  finished <- all(vlapply(processes, function(p) !p$is_alive()))
  
  any_not_done && finished
  
}

### PLACEHOLDER
revcheck_process <- function(revdep, reversecheck_dir, type, lib.loc) {
  callr::r_bg(function(revdep, reversecheck_dir, type, lib.loc) {
    path <- normalizePath(file.path(path_revdep(reversecheck_dir, revdep), type, "results.csv"), mustWork = FALSE)
    dir_create(dirname(path))
    utils::write.csv(as.data.frame(utils::installed.packages(lib.loc = lib.loc)), file = path)
    if (type == "new") {
      set_revdep_status(reversecheck_dir, revdep, status = "DONE")
    }
  }, args = list(
    revdep = revdep,
    reversecheck_dir = reversecheck_dir,
    type = type,
    lib.loc = lib.loc
  ), 
    package = "reversecheck"
  )
}
