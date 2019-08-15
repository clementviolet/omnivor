context("Testing network structual properties functions")

test_that("number of nodes", {
  expect_equal(nb_node(aleutian), 15)
})

test_that("number of interactions", {
  expect_equal(nb_edge(aleutian), 26)
})

test_that("linkage density", {
  expect_equal(link_density(aleutian), 26/15)
})

test_that("connectance", {
  expect_equal(connectance(aleutian), 26/15^2)
  expect_equal(connectance(aleutian, loops = FALSE), 26/(15*(15-1)))
  expect_equal(connectance(aleutian, directed = FALSE), 26/(15*(15+1)/2))
  expect_equal(connectance(aleutian, loops = FALSE, directed = FALSE), 26/(15*(15-1)/2))
})

test_that("degree distribution", {
  expect_equal(degree_distribution(aleutian), c('0' = 0, '1' = 0, '2' = 0.2, '3' = 0.4, '4' = 1/3, '5' = 0, '6' = 0, '7' = 0, '8' = 2/30))
})

test_that("diameter", {
  expect_equal(diameter(aleutian), 4)
})

test_that("clustering coefficient", {
  expect_equal(cl(aleutian), 0.1139241, tolerance = 1e-07)
})

test_that("nestedness", {
  expect_equal(nestedness(aleutian), 6/17)
  expect_equal(nestedness(aleutian, transpose = TRUE), 10/17)
  
  expect_equal(nestedness_total(aleutian), mean(6/17, 10/17))
})
