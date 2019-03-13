context("test-nb_edge")

test_that("count the number of edge works", {

  x <- igraph::graph(c(1,2,2,3,3,4), directed = FALSE)
  nb_igraph <- igraph::gsize(x)

  expect_equal(nb_edge(x), nb_igraph)
})
