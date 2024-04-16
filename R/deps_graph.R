#' Create Dependency Graph for Reverse Dependencies
#'
#' Discovers reverse dependencies and builds a dependency graph for all reverse
#' dependency dependencies.
#'
#' @param pkg A package whose reverse dependencies should be used as the basis
#'   for the dependency graph.
#' @inheritParams tools::package_dependencies
#' @inheritDotParams miniCRAN::makeDepGraph
#'
#' @importFrom tools package_dependencies
revdeps_graph_create <- function(
    pkg,
    db = utils::available.packages(),
    which = "strong",
    ...) {
  dep_graph_create(unlist(tools::package_dependencies(
    pkg,
    db = db,
    which = which,
    reverse = TRUE
  )), ...)
}

#' Create Dependency Graph
#'
#' @param pkg,... Passed to [miniCRAN::makeDepGraph]
#' @return A dependency graph with vertex attributes "root" (a logical value
#'   indicating whether the package as one of the roots used to create the
#'   graph), "status" (installation status) and "order" (installation order).
#'
#' @importFrom miniCRAN makeDepGraph
#' @importFrom igraph V
dep_graph_create <- function(pkg, ...) {
  statuses <- c("pending", "in progress", "done")
  g <- miniCRAN::makeDepGraph(pkg, ...)
  igraph::V(g)$root <- igraph::V(g)$name %in% pkg
  igraph::V(g)$status <- factor("pending", levels = statuses)
  g <- dep_graph_sort(g)
  g
}

#' Sort Dependency Graph by Strong Dependency Order
#'
#' @note
#' Cyclic dependencies are possible. Cyclic dependencies are disallowed for all
#' hard dependencies on CRAN today, though there have been historical instances
#' where they appeared on CRAN.
#'
#' Installation priority is based on:
#'   1. Total dependency footprint (low to high)
#'   2. Topology (leaf nodes first)
#'
#' @param g A [igraph::graph], expected to contain node attribute `type`.
#' @return The [igraph::graph] `g`, with vertices sorted in preferred
#'   installation order.
#'
#' @importFrom igraph vertex_attr neighborhood subgraph.edges permute topo_sort E V
dep_graph_sort <- function(g) {
  roots <- which(igraph::vertex_attr(g, "root"))

  # split into neighborhoods by root (revdep)
  nhood <- igraph::neighborhood(
    g,
    nodes = roots,
    order = length(g),
    mode = "in"
  )

  # prioritize by neighborhood size (small to large)
  priority <- length(nhood)
  priority_footprint <- integer(length(g))
  for (i in order(-vapply(nhood, length, integer(1L)))) {
    priority_footprint[nhood[[i]]] <- priority
    priority <- priority - 1
  }

  # use only strong dependencies to prioritize by topology (leafs first)
  strong_edges <- igraph::E(g)[igraph::E(g)$type %in% DEPENDENCIES_STRONG]
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
dep_graph_which_satisfied <- function(g, v = igraph::V(g), dependencies = TRUE, status = "pending") { # nolint
  dependencies <- check_dependencies(dependencies)
  if (length(status) > 0) {
    idx <- v$status == status
    v <- v[idx]
  }
  deps_met <- vlapply(
    igraph::incident_edges(g, v, mode = "in"),
    function(edges) {
      edges <- edges[edges$type %in% dependencies]
      all(igraph::tail_of(g, edges)$status == "done")
    }
  )
  names(deps_met[deps_met])
}

#' @describeIn dep_graph_which_satisfied
#' List vertices whose strong dependencies are satisfied
dep_graph_which_satisfied_strong <- function(..., dependencies = "strong") { # nolint
  dep_graph_which_satisfied(..., dependencies = dependencies)
}

#' @describeIn dep_graph_which_satisfied
#' List root vertices whose dependencies are all satisfied
dep_graph_which_root_satisfied <- function(g, ..., dependencies = "all", status = "done") {
  dep_graph_which_satisfied(
    g,
    igraph::V(g)[igraph::V(g)$root],
    ...,
    dependencies = dependencies,
    status = status
  )
}

dep_graph_set_package_status <- function(G, v, status) {
  igraph::set_vertex_attr(G, "status", v, factor(status, levels = c("pending", "in progress", "done")))
}

dep_graph_is_dependency <- function(G, v) {
  length(igraph::adjacent_vertices(G, v, "out")[[v]]) > 0
}

dep_graph_update_done <- function(G, lib.loc) {
  V <- igraph::V(G)

  which_done <- which(vlapply(V$name, function(p) {
    is_package_done(p, lib.loc)
  }))
  dep_graph_set_package_status(G, V[which_done], "done")
}
