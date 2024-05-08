empty_edge <- data.frame(
  dep = character(0),
  root = character(0),
  type = character(0)
)

base_pkgs <- function() {
  ip <- installed.packages()
  c("R", ip[!is.na(ip[, "Priority"]) & ip[, "Priority"] == "base", "Package"])
}

split_packages_names <- function(x) {
  unname(
    gsub(
      "^\\s+|\\s+$", "", 
      unlist(
        strsplit(gsub("\\s*\\(.*?\\)\\s*", "", x), ",\\s*")
        )
      )
    )
  
}

task_graph_create <- function(df, repos = getOption("repos"), ...) {
  edges <- task_edges_df(df, getOption("repos"))
  vertices <- task_vertices_df(df, edges)
  
  g <- igraph::graph_from_data_frame(edges, vertices = vertices)
  igraph::V(g)$status <- STATUS$pending
  g <- task_graph_sort(g)
  g
}

task_edges_df <- function(df, repos) {
  pkgs <- unique(vcapply(df$package, `[[`, "package"))
  db <- available.packages(repos = repos)[, c("Package", uulist(DEP))]
  
  # Add custom packages to db
  custom_pkgs_names <- uulist(lapply(df$custom, `[[`, "name"))
  custom_pkgs_paths <- uulist(lapply(df$custom, `[[`, "path"))
  # Need to use data frame to prevent dropping when assigning to 1 row matrix
  desc <- as.data.frame(read.dcf(file.path(custom_pkgs_paths, "DESCRIPTION")))
  desc <- desc[, intersect(colnames(db), colnames(desc)), drop = FALSE]
  desc[, "Package"] <- custom_pkgs_names
  desc[setdiff(colnames(db), colnames(desc))] <- NA_character_
  desc <- desc[, c("Package", uulist(DEP))]
  db <- rbind(db, desc)
  
  # Get recursively strong dependencies for all packages to check and custom packages
  core_dependencies <- tools::package_dependencies(c(pkgs, custom_pkgs_names), db = db, which = "strong", recursive = TRUE)
  # Get suggests end enhances dependencies first so we can derive hard dependencies for them as well
  suggests_dependencies <- uulist(tools::package_dependencies(pkgs, db = db, which = c("Suggests", "Enhances"), recursive = FALSE))
  # Get strong dependencies for suggested packages required for running R CMD check
  core_suggests_dependencies <- uulist(tools::package_dependencies(suggests_dependencies, db = db, which = "strong", recursive = TRUE))
  
  dependencies <- uulist(
    c(
      pkgs, # tools::package_dependencies do not include package itself, hence we add it at this stage
      custom_pkgs_names,
      core_dependencies, 
      suggests_dependencies, 
      core_suggests_dependencies
    )
  )
  dependencies <- dependencies[!dependencies %in% base_pkgs()]
  
  edges <- drlapply(dependencies, function(p) {
    edges_per_type <- drlapply(uulist(DEP), function(type) {
      deps <- try(db[db[, "Package"] == p, type], silent = TRUE)
      if (inherits(deps, "try-error") || length(deps) == 0) {
        empty_edge
      } else {
        deps <- split_packages_names(deps)
        deps <- deps[deps %in% dependencies]
        data.frame(
          dep = deps,
          root = rep(p, times = length(deps)),
          type = rep(type, times = length(deps))
        )
      }
    })
  })

  # Particular R CMD check tasks are separate nodes with aliases as names
  # We need to generate edges for them using original package dependencies
  # to make sure dependency relation is maintained
  check_edges <- drlapply(pkgs, function(p) {
    df_rows <- df[vcapply(df$package, `[[`, "package") == p, ]
    edges_rows <- edges[edges$root == p, ]
    
    data.frame(
      dep =  rep(edges_rows$dep, times = NROW(df_rows)),
      root = rep(df_rows$alias, each = NROW(edges_rows)),
      type =  rep(edges_rows$type, times = NROW(df_rows))
    )
  })

  # Generating edges for custom packages if applicable for given R CMD check alias.
  # Hardcoding Depends type to make sure it is always installed prior running the check
  custom_edges <- drlapply(df$alias, function(a) {
    row <- df[df$alias == a, ]
    if (!is.null(row$custom[[1]]$name)) {
      data.frame(
        dep = row$custom[[1]]$name,
        root = row$alias,
        type = "Depends"
      )
    } else {
      empty_edge
    }
  })
  
  # Droping original edges for final packages (scheduled to be checked)
  # as we are about to append check edges.
  # Certain packages can be both, a dependency and a package to check
  roots_to_remove <- pkgs[pkgs %in% edges$root & !pkgs %in% edges$dep]
  edges <- edges[!edges$root %in% roots_to_remove, ]
  
  edges <- rbind(edges, check_edges, custom_edges)
  edges
}

task_vertices_df <- function(df, edges) {
  vertices <- unique(c(edges$dep, edges$root))
  custom_pkgs_names <- uulist(lapply(df$custom, `[[`, "name"))
  pkgs <- unique(vcapply(df$package, `[[`, "package"))
  task_type <- ifelse(vertices %in% df$alias, "check", "install")
  custom <- vertices %in% custom_pkgs_names
  install_lib <- vcapply(vertices, function(x) {
    if (x %in% df$alias) {
      sprintf("path_check_output(private$output, \"%s\")", x)
    } else if (x %in% custom_pkgs_names) {
      sprintf("path_custom_lib(private$output, \"%s\")", x)
    } else {
      "path_lib(private$output)"
    }
  })
  
  
  data.frame(
    name = vertices,
    type = task_type,
    install_lib = install_lib,
    custom = custom
  )
}

task_get_lib_loc <- function(g, node) {
  nhood <- task_graph_neighborhoods(g, node)[[1]]
  nhood <- nhood[names(nhood) != node]
  # Custom packages are possible only for the check type nodes which are
  # always terminal. Therefore if we sort nhood making custom packages appear
  # first, their lib will always be prioritized
  attributes <- igraph::vertex.attributes(g, index = nhood)
  unique(attributes$install_lib[order(attributes$custom, decreasing = TRUE)])
}

#' Find Dependency Neighborhood
#'
#' @param g A dependency graph, as produced with [dep_graph_create()]
#' @param names Names of packages whose neighborhoods should be calculcated.
#'
#' @importFrom igraph neighboorhood V
task_graph_neighborhoods <- function(g, nodes) {
  igraph::neighborhood(
    g,
    order = length(g),
    nodes = nodes,
    mode = "in"
  )
}


task_graph_sort <- function(g) {
  roots <- which(igraph::vertex_attr(g, "type") == "check")
  
  # split into neighborhoods by root (revdep)
  nhood <- task_graph_neighborhoods(g, roots)
  
  # prioritize by neighborhood size (small to large)
  priority <- length(nhood)
  priority_footprint <- integer(length(g))
  for (i in order(-vapply(nhood, length, integer(1L)))) {
    priority_footprint[nhood[[i]]] <- priority
    priority <- priority - 1
  }
  
  # use only strong dependencies to prioritize by topology (leafs first)
  strong_edges <- igraph::E(g)[igraph::E(g)$type %in% DEP_STRONG]
  g_strong <- igraph::subgraph.edges(g, strong_edges, delete.vertices = FALSE)
  topo <- igraph::topo_sort(g_strong, mode = "in")
  priority_topo <- integer(length(g))
  priority_topo[match(topo$name, igraph::V(g)$name)] <- rev(seq_along(topo))
  
  # combine priorities, prioritize first by total, footprint then topology
  priorities <- rbind(priority_footprint, priority_topo)
  order <- rank(length(igraph::V(g))^seq(nrow(priorities) - 1, 0) %*% priorities)
  g <- igraph::permute(g, order)
  
  g
}

#' Find the Next Packages Not Dependent on an Unavailable Package
#'
#' While other packages are in progress, ensure that the next selected package
#' already has its dependencies done.
#'
#' @param g A dependency graph, as produced with [dep_graph_create()]
#' @return The name of the next package to prioritize
#'
#' @importFrom igraph incident_edges tail_of
task_graph_which_satisfied <- function(
    g,
    v = igraph::V(g),
    dependencies = TRUE,
    status = STATUS$pending) {
  if (is.character(status)) status <- STATUS[[status]]
  dependencies <- check_dependencies(dependencies)
  if (length(status) > 0) {
    idx <- v$status %in% status
    v <- v[idx]
  }
  deps_met <- vlapply(
    igraph::incident_edges(g, v, mode = "in"),
    function(edges) {
      edges <- edges[edges$type %in% dependencies]
      edges <<- edges
      all(igraph::tail_of(g, edges)$status == STATUS$done)
    }
  )
  names(deps_met[deps_met])
}

#' @describeIn dep_graph_which_satisfied
#' List vertices whose strong dependencies are satisfied
task_graph_which_satisfied_strong <- function(..., dependencies = "strong") { # nolint
  task_graph_which_satisfied(..., dependencies = dependencies)
}

#' @describeIn dep_graph_which_satisfied
#' List root vertices whose dependencies are all satisfied
task_graph_which_check_satisfied <- function(
    g,
    ...,
    dependencies = "all",
    status = "done") {
  task_graph_which_satisfied(
    g,
    igraph::V(g)[igraph::V(g)$type == "check"],
    ...,
    dependencies = dependencies,
    status = status
  )
}

rev_dep_check_tasks_df <- function(path, repos = getOption("repos")) {
  path <- check_path_is_pkg_source(path)
  package <- get_package_name(path)
  package_v <- available.packages()[package, "Version"]
  revdeps <- tools::package_dependencies(package, reverse = TRUE)[[1]]
  version <- available.packages()[revdeps, "Version"]
  
  df_dev <- df_rel <- data.frame(
    alias = revdeps,
    version = version
  )
  
  df_dev$package <- df_rel$package <- rev_dep_check_tasks(revdeps, repos)
  df_dev$custom <- rep(list(list(
    name = paste0(package, " (dev)"),
    package = package,
    path = path,
    custom = TRUE
  )), times = NROW(df_dev))
  
  df_rel$custom <- rep(list(list()), times = NROW(df_dev))
  
  df_dev$alias <- paste0(df_dev$alias, " (dev)")
  df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
  idx <- rep(seq_len(nrow(df_rel)), each = 2) + c(0, nrow(df_rel))
  rbind(df_dev, df_rel)[idx, ]
}

rev_dep_check_tasks <- function(packages, repos) {
  db <- available.packages(repos = repos)
  lapply(packages, function(p) {
    list(
      package = p,
      path = get_package_source(p, db = db),
      custom = FALSE
    )
  })
}