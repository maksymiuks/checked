empty_edge <- data.frame(
  dep = character(0),
  root = character(0),
  type = character(0)
)

task_graph_create <- function(df, repos = getOption("repos"), ...) {
  edges <- task_edges_df(df, repos)
  vertices <- task_vertices_df(df, edges, repos)
  
  g <- igraph::graph_from_data_frame(edges, vertices = vertices)
  igraph::V(g)$status <- STATUS$pending
  g <- task_graph_sort(g)
  g
}

task_edges_df <- function(df, repos) {
  pkgs_raw <- vcapply(df$package, `[[`, "name")
  pkgs <- unique(pkgs_raw)
  db <- available.packages(repos = repos)[, c("Package", uulist(DEP))]
  
  # Add custom packages to db
  custom_pkgs_aliases <- uulist(lapply(df$custom, `[[`, "alias"))
  custom_packages_names_map <- data.frame(
    value = custom_pkgs_aliases,
    hash = vcapply(custom_pkgs_aliases, raw_based_hash)
  )
  custom_pkgs_paths <- uulist(lapply(df$custom, `[[`, "path"))
  # Need to use data frame to prevent dropping when assigning to 1 row matrix
  desc <- as.data.frame(read.dcf(file.path(custom_pkgs_paths, "DESCRIPTION")))
  desc <- desc[, intersect(colnames(db), colnames(desc)), drop = FALSE]
  desc[, "Package"] <- custom_packages_names_map$hash
  desc[setdiff(colnames(db), colnames(desc))] <- NA_character_
  desc <- desc[, c("Package", uulist(DEP))]
  # Adding checks to db and custom packages as Depends link
  checks <- db[pkgs_raw, ]
  custom_pkgs_aliases_raw <- replace_with_map(as.character(lapply(df$custom, `[[`, "alias")), custom_packages_names_map$value, custom_packages_names_map$hash)
  checks[, "Depends"] <- ifelse(
    test = custom_pkgs_aliases_raw == "NULL", 
    yes = checks[, "Depends"],
    no = ifelse(is.na(checks[, "Depends"]), custom_pkgs_aliases_raw, paste0(checks[, "Depends"], ", ", custom_pkgs_aliases_raw))
  )
  checks[, "Package"] <- df$alias
  db <- rbind(db, desc, checks)

  # Get suggests end enhances dependencies first so we can derive hard dependencies for them as well
  suggests_dependencies <- uulist(tools::package_dependencies(df$alias, db = db, which = c("Suggests", "Enhances"), recursive = FALSE))  
  # Get recursively strong dependencies for all packages 
  core_dependencies <- tools::package_dependencies(c(df$alias, custom_pkgs_aliases, suggests_dependencies), db = db, which = "strong", recursive = TRUE)
  
  dependencies <- uulist(
    c(
      df$alias, # tools::package_dependencies do not include package itself, hence we add it at this stage
      custom_pkgs_aliases,
      suggests_dependencies, 
      core_dependencies
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
  
  edges$dep <- replace_with_map(edges$dep, custom_packages_names_map$hash, custom_packages_names_map$value)
  edges$root <- replace_with_map(edges$root, custom_packages_names_map$hash, custom_packages_names_map$value)
  edges
}

task_vertices_df <- function(df, edges, repos) {
  vertices <- unique(c(edges$dep, edges$root))
  custom_pkgs_aliases <- uulist(lapply(df$custom, `[[`, "alias"))
  pkgs <- unique(vcapply(df$package, `[[`, "name"))
  task_type <- ifelse(vertices %in% df$alias, "check", "install")

  
  packages <- lapply(vertices, function(v) {
    if (v %in% df$alias) {
      v_pkg <- df$package[[which(df$alias == v)]]
    } else if (v %in% custom_pkgs_aliases) {
      df$custom[[head(which(as.character(lapply(df$custom, `[[`, "alias")) == v), 1)]]
    } else {
      reversecheck_package(
        name = v,
        repos = repos
      )
    }
  })
  
  out <- data.frame(
    name = vertices,
    type = task_type,
    custom = vertices %in% custom_pkgs_aliases
  )
  
  out$package <- packages
  
  out
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
    status = STATUS$pending) {
  task_graph_which_satisfied(
    g,
    igraph::V(g)[igraph::V(g)$type == "check"],
    ...,
    dependencies = dependencies,
    status = status
  )
}

task_graph_set_package_status <- function(g, v, status) {
  if (is.character(status)) status <- STATUS[[status]]
  igraph::set_vertex_attr(g, "status", v, status)
}

task_graph_update_done <- function(g, lib.loc) {
  v <- igraph::V(g)[igraph::V(g)$type == "install"]
  which_done <- which(vlapply(v$name, is_package_done, lib.loc = lib.loc))
  task_graph_set_package_status(g, v[which_done], STATUS$done)
}
