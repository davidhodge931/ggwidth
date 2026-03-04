library(testthat)
library(ggplot2)
library(grid)

# Helper to set a standard theme for tests
set_standard_theme <- function() {
  set_theme(
    theme_grey() +
      theme(panel.widths  = unit(75, "mm")) +
      theme(panel.heights = unit(50, "mm"))
  )
}

test_that("get_width() errors if n is NULL", {
  set_standard_theme()
  expect_error(get_width(), "n must be specified")
})

test_that("get_width() errors if panel_widths is not a unit object", {
  set_standard_theme()
  expect_error(get_width(n = 3, panel_widths = 75), "`panel_widths` must be a `grid::unit` object.")
})

test_that("get_width() errors if panel_heights is not a unit object", {
  set_standard_theme()
  expect_error(get_width(n = 3, panel_heights = 50), "`panel_heights` must be a `grid::unit` object.")
})

test_that("get_width() errors if panel dimensions are not set in theme", {
  set_theme(theme_grey())
  expect_error(get_width(n = 3), "Physical panel widths and heights must both be set in the theme.")
})

test_that("get_width() returns a single numeric value", {
  set_standard_theme()
  result <- get_width(n = 3)
  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("get_width() returns a value less than 1", {
  set_standard_theme()
  expect_lt(get_width(n = 3), 1)
})

test_that("get_width() errors when calculated width >= 1", {
  set_standard_theme()
  expect_error(
    get_width(n = 3, equiwidth = 100),
    "The calculated width must be less than 1"
  )
})

test_that("get_width() returns larger width for more categories", {
  set_standard_theme()
  expect_lt(get_width(n = 3), get_width(n = 7))
})

test_that("get_width() returns larger width for larger equiwidth", {
  set_standard_theme()
  expect_gt(get_width(n = 3, equiwidth = 1.5), get_width(n = 3, equiwidth = 1))
})

test_that("get_width() returns larger width for larger n_dodge", {
  set_standard_theme()
  expect_lt(get_width(n = 3, n_dodge = 1), get_width(n = 3, n_dodge = 4))
})

test_that("get_width() returns smaller width for wider panels", {
  set_standard_theme()
  expect_gt(
    get_width(n = 3, panel_widths = unit(75, "mm")),
    get_width(n = 3, panel_widths = unit(150, "mm"))
  )
})

test_that("get_width() returns smaller width for taller panels with y orientation", {
  set_standard_theme()
  expect_gt(
    get_width(n = 3, orientation = "y", panel_heights = unit(50, "mm")),
    get_width(n = 3, orientation = "y", panel_heights = unit(100, "mm"))
  )
})

test_that("get_width() x and y orientation differ", {
  set_standard_theme()
  expect_false(
    isTRUE(all.equal(
      get_width(n = 3, orientation = "x"),
      get_width(n = 3, orientation = "y")
    ))
  )
})

test_that("get_width() n_dodge = 1 matches default (no n_dodge supplied)", {
  set_standard_theme()
  expect_equal(get_width(n = 3, n_dodge = 1), get_width(n = 3))
})

test_that("get_width() reference case returns expected value", {
  set_standard_theme()
  set_equiwidth(1)
  expect_equal(get_width(n = 3), 1 / 7.5, tolerance = 1e-6)
})

test_that("set_equiwidth() affects get_width() when equiwidth is NULL", {
  set_standard_theme()
  set_equiwidth(1)
  w1 <- get_width(n = 3)
  set_equiwidth(2)
  w2 <- get_width(n = 3)
  expect_gt(w2, w1)
  set_equiwidth(1) # reset
})

test_that("get_width() explicit equiwidth overrides set_equiwidth()", {
  set_standard_theme()
  set_equiwidth(2)
  expect_equal(
    get_width(n = 3, equiwidth = 1),
    get_width(n = 3, equiwidth = 1)
  )
  set_equiwidth(1) # reset
})

test_that("get_width() panel_widths argument overrides theme panel widths", {
  set_standard_theme()
  expect_false(
    isTRUE(all.equal(
      get_width(n = 3),
      get_width(n = 3, panel_widths = unit(150, "mm"))
    ))
  )
})

test_that("get_width() panel_heights argument overrides theme panel heights", {
  set_standard_theme()
  expect_false(
    isTRUE(all.equal(
      get_width(n = 3, orientation = "y"),
      get_width(n = 3, orientation = "y", panel_heights = unit(100, "mm"))
    ))
  )
})

test_that("get_width() equal panel_widths vector is accepted", {
  set_standard_theme()
  expect_no_error(
    get_width(n = 3, panel_widths = unit(c(75, 75), "mm"))
  )
})

test_that("set_equiwidth() sets the global option correctly", {
  set_equiwidth(1.5)
  expect_equal(getOption("ggwidth.equiwidth"), 1.5)
  set_equiwidth(1) # reset
})

test_that("set_equiwidth() defaults to 1", {
  set_equiwidth()
  expect_equal(getOption("ggwidth.equiwidth"), 1)
})
