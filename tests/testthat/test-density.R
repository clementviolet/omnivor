context("test-density")

test_that("density works", {

  x <- igraph::graph(c(1,2,2,3,3,4), directed = FALSE)
  density_igraph <- igraph::gsize(x)/igraph::gorder(x)

  expect_equal(link_density(x), density_igraph)
})
