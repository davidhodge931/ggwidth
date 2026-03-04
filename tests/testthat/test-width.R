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
  expect_error(get_width(), "`n` must be specified.")
})

test_that("get_width() errors if n is not a whole number", {
  set_standard_theme()
  expect_error(get_width(n = 3.5), "`n` must be a single whole number.")
})

test_that("get_width() errors if n is not a scalar", {
  set_standard_theme()
  expect_error(get_width(n = c(3, 4)), "`n` must be a single whole number.")
})

test_that("get_width() errors if n_dodge is not a whole number", {
  set_standard_theme()
  expect_error(get_width(n = 3, n_dodge = 1.5), "`n_dodge` must be a single whole number.")
})

test_that("get_width() errors if equiwidth is not a finite numeric value", {
  set_standard_theme()
  expect_error(get_width(n = 3, equiwidth = Inf), "`equiwidth` must be a single finite numeric value.")
})

test_that("get_width() errors if equiwidth is not a scalar", {
  set_standard_theme()
  expect_error(get_width(n = 3, equiwidth = c(1, 2)), "`equiwidth` must be a single finite numeric value.")
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

test_that("get_width() errors if panel_widths elements are not equal", {
  set_standard_theme()
  expect_error(
    get_width(n = 3, panel_widths = unit(c(75, 50), "mm")),
    "equal"
  )
})

test_that("get_width() errors if panel_heights elements are not equal", {
  set_standard_theme()
  expect_error(
    get_width(n = 3, panel_heights = unit(c(50, 75), "mm")),
    "equal"
  )
})

test_that("get_width() errors if orientation is invalid", {
  set_standard_theme()
  expect_error(get_width(n = 3, orientation = "z"))
})

test_that("get_width() errors if ... is not empty", {
  set_standard_theme()
  expect_error(get_width(n = 3, foo = 1))
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
  expect_gt(get_width(n = 3, equiwidth = 1.5), get_width(n = 3, equiwidth = 1.0))
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

test_that("get_width() equal panel_widths vector is accepted", {
  set_standard_theme()
  expect_no_error(
    get_width(n = 3, panel_widths = unit(c(75, 75), "mm"))
  )
})

test_that("set_equiwidth() affects get_width() when equiwidth is NULL", {
  set_standard_theme()
  withr::local_options(ggwidth.equiwidth = 1)
  w1 <- get_width(n = 3)
  withr::local_options(ggwidth.equiwidth = 2)
  w2 <- get_width(n = 3)
  expect_gt(w2, w1)
})

test_that("get_width() explicit equiwidth overrides set_equiwidth()", {
  set_standard_theme()
  withr::local_options(ggwidth.equiwidth = 2)
  expect_equal(
    get_width(n = 3, equiwidth = 1.0),
    get_width(n = 3, equiwidth = 1.0)
  )
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

test_that("set_equiwidth() sets the global option correctly", {
  withr::local_options(ggwidth.equiwidth = 1)
  set_equiwidth(1.5)
  expect_equal(getOption("ggwidth.equiwidth"), 1.5)
})

test_that("set_equiwidth() defaults to 1", {
  withr::local_options(ggwidth.equiwidth = NULL)
  set_equiwidth()
  expect_equal(getOption("ggwidth.equiwidth"), 1)
})

# Exact value tests
test_that("get_width() reference case: n=3, x, 75mm", {
  set_standard_theme()
  withr::local_options(ggwidth.equiwidth = 1)
  expect_equal(get_width(n = 3), 1 / 7.5, tolerance = 1e-6)
})

test_that("get_width() n=7, x, 75mm", {
  set_standard_theme()
  withr::local_options(ggwidth.equiwidth = 1)
  expect_equal(get_width(n = 7), (7 / 3) / 7.5, tolerance = 1e-6)
})

test_that("get_width() n=3, equiwidth=2, x, 75mm", {
  set_standard_theme()
  expect_equal(get_width(n = 3, equiwidth = 2.0), 2 / 7.5, tolerance = 1e-6)
})

test_that("get_width() n=3, n_dodge=4, x, 75mm", {
  set_standard_theme()
  withr::local_options(ggwidth.equiwidth = 1)
  expect_equal(get_width(n = 3, n_dodge = 4), 4 / 7.5, tolerance = 1e-6)
})

test_that("get_width() n=3, y orientation, 75mm x 50mm", {
  set_standard_theme()
  withr::local_options(ggwidth.equiwidth = 1)
  # ref_n for y = 3 * (50/75) = 2
  expect_equal(get_width(n = 3, orientation = "y"), (3 / 2) / 7.5, tolerance = 1e-6)
})

test_that("get_width() n=3, x, panel_widths=150mm", {
  set_standard_theme()
  withr::local_options(ggwidth.equiwidth = 1)
  # scaling_factor = 75/150 = 0.5
  expect_equal(
    get_width(n = 3, panel_widths = unit(150, "mm")),
    (1 / 7.5) * (75 / 150),
    tolerance = 1e-6
  )
})

test_that("get_width() n=3, y, panel_heights=100mm", {
  set_standard_theme()
  withr::local_options(ggwidth.equiwidth = 1)
  # ref_n for y = 3 * (50/75) = 2, scaling_factor = 50/100 = 0.5
  expect_equal(
    get_width(n = 3, orientation = "y", panel_heights = unit(100, "mm")),
    ((3 / 2) / 7.5) * (50 / 100),
    tolerance = 1e-6
  )
})

test_that("get_width() n=5, n_dodge=2, equiwidth=1.5, x, 75mm", {
  set_standard_theme()
  expect_equal(
    get_width(n = 5, n_dodge = 2, equiwidth = 1.5),
    (5 / 3) * (1.5 / 7.5) * (2 / 1),
    tolerance = 1e-6
  )
})
