context("test-food-web-strategy")

test_that("genrality functions", {
  expect_equal(gen(aleutian)$generality, c(1, 1, 1, 1, 3, 4, 2, 5, 1, 3, 3, 1, 0, 0, 0))
  
  expect_equal(gen_sd(aleutian)$generality, c(0.5769231, 0.5769231, 0.5769231, 0.5769231, 1.7307692, 2.3076923, 1.1538462, 2.8846154, 0.5769231, 1.7307692, 1.7307692, 0.5769231, 0, 0, 0), tolerance = 1e-7)
  
  expect_equal(gen_mean(aleutian), 1.733333, tolerance = 1e-6)
})

test_that("vulnerability functions", {
  expect_equal(vul(aleutian)$vulnerability, c(2, 2, 2, 2, 1, 0, 2, 3, 1, 1, 0, 1, 2, 4, 3))
  
  expect_equal(vul_sd(aleutian)$vulnerability, c(1.1538462, 1.1538462, 1.1538462, 1.1538462, 0.5769231, 0, 1.1538462, 1.7307692, 0.5769231, 0.5769231, 0, 0.5769231, 1.1538462, 2.3076923, 1.7307692), tolerance = 1e-7)
  
  expect_equal(vul_mean(aleutian), 1.733333, tolerance = 1e-6)
})

test_that("link functions", {
  expect_equal(link(aleutian)$nb_link, c(3, 3, 3, 3, 4, 4, 4, 8, 2, 4, 3, 2, 2, 4, 3))
  
  expect_equal(link_sd(aleutian)$nb_link, c(1.730769, 1.730769, 1.730769, 1.730769, 2.307692, 2.307692, 2.307692, 4.615385, 1.153846, 2.307692, 1.730769, 1.153846, 1.153846, 2.307692, 1.730769), tolerance = 1e-6)
  
  expect_equal(link_mean(aleutian), 3.466667, tolerance = 1e-6)
})