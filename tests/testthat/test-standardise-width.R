library(ggplot2)

set_theme(
  new = theme_grey() +
    theme(panel.widths = rep(unit(50, "mm"), 2)) +
    theme(panel.heights = rep(unit(75, "mm"), 2))
)

test_that("standardise_width returns correct values with default standard (0.2)", {
  expect_equal(standardise_width(n = 3, dodge_n = 1, orientation = "x"),         0.3)
  expect_equal(standardise_width(n = 2, dodge_n = 3, orientation = "x"),         0.6)
  expect_equal(standardise_width(n = 2, dodge_n = 3, orientation = "y"),         0.4)
  expect_equal(standardise_width(n = 5, dodge_n = 1, orientation = "y"),         1/3, tolerance = 1e-6)
})

test_that("standardise_width returns correct values with scale = 0.1", {
  expect_equal(standardise_width(n = 3, dodge_n = 1, orientation = "x", scale = 0.5), 0.15)
  expect_equal(standardise_width(n = 2, dodge_n = 3, orientation = "x", scale = 0.5), 0.3)
  expect_equal(standardise_width(n = 2, dodge_n = 3, orientation = "y", scale = 0.5), 0.2)
  expect_equal(standardise_width(n = 5, dodge_n = 1, orientation = "y", scale = 0.5), 1/6, tolerance = 1e-6)
})

test_that("standardise_width returns correct values with scale = 0.3", {
  expect_equal(standardise_width(n = 3, dodge_n = 1, orientation = "x", scale = 1.5), 0.45)
  expect_equal(standardise_width(n = 2, dodge_n = 3, orientation = "x", scale = 1.5), 0.9)
  expect_equal(standardise_width(n = 2, dodge_n = 3, orientation = "y", scale = 1.5), 0.6)
  expect_equal(standardise_width(n = 5, dodge_n = 1, orientation = "y", scale = 1.5), 0.5)
})
