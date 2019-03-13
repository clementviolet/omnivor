context("test-nb_node")

test_that("count the number of node works", {

  x <- igraph::graph(c(1,2,2,3,3,4), directed = FALSE)
  nb_igraph <- igraph::gorder(x)

  expect_equal(nb_node(x), nb_igraph)
})
