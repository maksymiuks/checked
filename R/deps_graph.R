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
    db = available.packages(),
    which = "strong",
    ...) {
  revdeps <- tools::package_dependencies(
    pkg,
    db = db,
    which = which,
    recursive = TRUE
  )

  dep_graph_create(revdeps, ...)
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
  statuses <- c("pending", "installing", "installed")
  g <- miniCRAN::makeDepGraph(pkg, ...)
  igraph::V(g)$root <- igraph::V(g)$name %in% pkg
  igraph::V(g)$status <- factor("pending", levels = statuses)
  g <- dep_graph_update_install_order(g)
  g
}

#' Add Installation Order to Dependency Graph
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
#' @return The [igraph::graph] `g`, with additional `order` vertex attribute,
#'   indicating the order in which dependencies should be installed.
#'
#' @importFrom igraph vertex_attr neighborhood subgraph.edges topo_sort E V
dep_graph_update_install_order <- function(g) {
  strong_deps <- c("Depends", "Imports", "LinkingTo")
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
  strong_edges <- igraph::E(g)[igraph::E(g)$type %in% strong_deps]
  g_strong <- igraph::subgraph.edges(g, strong_edges, delete.vertices = FALSE)
  topo <- igraph::topo_sort(g_strong, mode = "in")
  priority_topo <- integer(length(g))
  priority_topo[match(topo$name, igraph::V(g)$name)] <- rev(seq_along(topo))

  # combine priorities, prioritize first by total, footprint then topology
  priorities <- rbind(priority_footprint, priority_topo)
  order <- rank(length(igraph::V(g))^seq(nrow(priorities) - 1, 0) %*% priorities)
  igraph::vertex_attr(g, "order") <- order

  g
}
