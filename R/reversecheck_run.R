reversecheck_run <- function(pkg, reversecheck_dir, lib.loc, n_childs, repos, 
                             rcmdcheck_params, ...) {
  
  revdeps <- get_revdeps_from_dir(reversecheck_dir)
  G <- dep_graph_create(revdeps[!revdeps$status %in% c("IN_PROGRESS", "DONE")])
  processes <- list()
  
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
    
    G <- dep_graph_update_install_order(G)
    next_packages <- dep_graph_next_packages(G)
    
    
    processes <- if (NROW(ready) > 0) {
      pkg <- ready[1, ]
      process <- if(pkg$status == "READY") {
        set_revdep_status(reversecheck_dir, pkg$package, "IN_PROGRESS_OLD")
        revcheck_process("OLD")
      } else {
        set_revdep_status(reversecheck_dir, pkg$package, "IN_PROGRESS")
        revcheck_process("NEW")
      }
      append(processes, process)
    } else if (any(todo$package %in% next_packages)) {
      
      pkg <- todo[which(todo$package %in% next_packages)[1], ]
      # revdeps that are not dependencies for any other dependencies can
      # be immediately marked as installed 
      if (!dep_graph_is_dependency(G, pkg)) {
        dep_graph_set_package_status(G, pkg, "installed")
      }
      
      set_revdep_status(reversecheck_dir, pkg$package, "PREPARING")
      process <- prepare_filesystem_process$new(
        pkg,
        reversecheck_dir,
        repos = repos,
        lib.loc = lib.loc
      )
      append(processes, process) 
    } else {
      for (pkg in next_packages) {
        status <- igraph::vertex.attributes(G, pkg)$status
        pkg_installed <- is_package_installed(pkg, lib.loc)
        if (status == "pending" && !pkg_installed) {
          break()
        } else if (status %in% c("pending", "installing") && pkg_installed) {
          dep_graph_set_package_status(G, pkg, "installed")
        }
      }
      
      # If the loop finished and the last value of status is different than pending,
      # it means there is no more packages to install in this iteration of the
      # scheduler
      if (status != "pending") next()
      
      dep_graph_set_package_status(G, pkg, "installing")
      process <- install_packages(
        pkgs = pkg, 
        lib = path_lib(reversecheck_dir, "cache"), 
        keep_outputs = file.path(path_logs(reversecheck_dir, "cache"), make.names(pkg)),
        repos = path_cache_repo(reversecheck_dir, TRUE),
        lib.loc = lib.loc, 
        logs_path = file.path(path_logs(reversecheck_dir, "cache"), make.names(pkg), "subprocess.log"),
        async = TRUE
      )
      append(processes, process) 
    }
  }
}


scheduler_loop_finished <- function(revdeps, processes) {
  # To stop the loop we want all revdeps to have "DONE" status
  # and all processes to finish
  done <- all(revdeps$status %in% "DONE")
  finished <- all(vlapply(processes, function(p) !p$is_alive()))
  
  done && finished
}

foo <- function() {
  TRUE
}