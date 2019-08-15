context("Testing species role function ")

test_that("percentage basal species", {
  expect_equal(basal(aleutian), 3/15*100)
})

test_that("percentage of top species", {
  expect_equal(top(aleutian), 2/15*100)
})

test_that("percentage of intermediate species", {
  expect_equal(inter(aleutian), 100 - 5/15*100)
})

test_that("percentage herbivor/detrivor species", {
  expect_equal(herbivory(aleutian), 7/15*100)
})

test_that("percentage canibal species", {
  expect_equal(can(aleutian), 0)
})

test_that("percentage omnivore species",{
  expect_equal(omnivore(aleutian, method = "shortest"), 1/4*100)
  expect_equal(omnivore(aleutian, method = "average_prey"), 1/3*100)
  
  expect_equal(omnivore_short_tl(aleutian)$Omnivore, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(omnivore_avg_prey_tl(aleutian)$Omnivore, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
  
  expect_equal(which_omnivore(aleutian, method = "shortest")$Omnivore, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(which_omnivore(aleutian, method = "average_prey")$Omnivore, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
})

test_that("shortest trophic lenght", {
  expect_equal(short_tl(aleutian)$trophic_level, c(2, 3, 4, 1, 3, 2, 1, 2, 4, 2, 2, 1, 2, 2, 3))
})

test_that("prey average trophic length", {
  expect_equal(prey_avg_tl(aleutian)$trophic_level, c(2, 2, 2, 2, 2, 3, 2.5, 3.1, 4.1, 3.533333, 4.577778, 2, 1, 1, 1), tolerance = 1e-7)
})

test_that("short weighted trophic level", {
  expect_equal(short_wght_tl(aleutian)$trophic_level, c(2, 2.5, 3, 1.5, 2.5, 2.5, 1.75, 2.55, 4.05, 2.766667, 3.288889, 1.5, 1.5, 1.5, 2), tolerance = 1e-7)
})


test_that("mean food chain length", {
  expect_equal(mean_tl(aleutian, method = "shortest"), 2.266667, tolerance = 1e-6)
  expect_equal(mean_tl(aleutian, method = "average_prey"), 2.387407, tolerance = 1e-6)
  expect_equal(mean_tl(aleutian, method = "weighted"), 2.327037, tolerance = 1e-6)
})