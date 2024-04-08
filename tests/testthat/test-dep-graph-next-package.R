test_that("dep_graph_next_package finds next installable package", {
  # nolint start, styler: off
  g <- igraph::make_graph(~
    A +- B +- C,
    A +------ D,
    A +- E +- D,
    A +- F +- D
  )
  # nolint end, styler: on

  # initialize graph characteristics to mock dep graph
  E(g)$type <- "Depends"
  V(g)$root <- V(g)$name == "A"
  expect_silent(g <- dep_graph_update_install_order(g))
  expect_equal(V(g)$name, c("D", "C", "F", "E", "B", "A"))

  # initialize graph, such that "D" is not completed when "E"  would be next
  # by order due to "D" having a long install time
  V(g)$status <- "pending"
  V(g)["D"]$status <- "installing"
  V(g)["C"]$status <- "installed"
  expect_equal(dep_graph_next_packages(g), "B")

  # if the order is reversed, now "F" and "E" should be next
  V(g)["D"]$status <- "installed"
  V(g)["C"]$status <- "installing"
  expect_equal(dep_graph_next_packages(g), c("F", "E"))
})
