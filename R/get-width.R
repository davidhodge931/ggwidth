#' Standardise 'ggplot2' geom width
#'
#' @description
#' Standardise the width in 'ggplot2' geoms to appear visually consistent across plots with different numbers of categories, panel dimensions, and orientations.
#'
#' This can be used in geoms such as [geom_bar()]/[geom_col()], [geom_boxplot()], [geom_errorbar()].
#'
#' @param ... Must be empty. Forces all other arguments to be named.
#' @param n Number of categories in the orientation aesthetic (i.e. `"x"` or `"y"`).
#'   For faceted plots, use the maximum `n` within a facet.
#' @param n_dodge Number of dodge categories. Must match the number of groups in
#'   the `fill` or `colour` aesthetic when using `position_dodge()`.
#' @param orientation Orientation: `"x"` for vertical (width appearance equiwidth to
#'   panel width), `"y"` for horizontal (width appearance equiwidth to panel height).
#' @param equiwidth Numeric. Scaling factor that controls the width appearance.
#'   A value of `1` is the default. Increase to make a wider appearance, and
#'   decrease to make a thinner appearance. If `NULL`, uses the value set by `set_equiwidth()`,
#'   falling back to `1`.
#' @param panel_widths A `grid::unit` object specifying the panel width. If `NULL`
#'   (default), uses the value set in the current theme.
#' @param panel_heights A `grid::unit` object specifying the panel height. If `NULL`
#'   (default), uses the value set in the current theme.
#'
#' @return A numeric width value passed to the `width` argument of
#'   `geom_bar()`, `geom_col()`, or similar geoms.
#'
#' @export
#'
#' @seealso [set_equiwidth()]
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(patchwork)
#'
#' set_theme(
#'   theme_grey() +
#'     theme(panel.widths  = rep(unit(75, "mm"), 2)) +
#'     theme(panel.heights = rep(unit(50, "mm"), 2))
#' )
#' set_equiwidth(1)
#'
#' p1 <- mpg |>
#'   ggplot(aes(x = drv)) +
#'   geom_bar(
#'     width = get_width(n = 3),
#'     colour = "black",
#'     fill = "grey",
#'   )
#'
#' p2 <- diamonds |>
#'   ggplot(aes(x = color)) +
#'   geom_bar(
#'     width = get_width(n = 7),
#'     colour = "black",
#'     fill = "grey",
#'   )
#'
#' p3 <- diamonds |>
#'   ggplot(aes(y = color)) +
#'   geom_bar(
#'     width = get_width(n = 7, orientation = "y"),
#'     colour = "black",
#'     fill = "grey",
#'   )
#'
#' p4 <- mpg |>
#'   ggplot(aes(x = drv, group = factor(cyl))) +
#'   geom_bar(
#'     position = position_dodge(preserve = "single"),
#'     width = get_width(n = 3, n_dodge = 4),
#'     colour = "black",
#'     fill = "grey",
#'   )
#'
#' p1 + p2 + p3 + p4
#'
#' d <- tibble::tibble(
#'   continent = c("Europe", "Europe", "Europe", "Europe", "Europe",
#'                 "South America", "South America"),
#'   country   = c("AT", "DE", "DK", "ES", "PK", "TW", "BR"),
#'   value     = c(10L, 15L, 20L, 25L, 17L, 13L, 5L)
#' )
#'
#' max_n <- d |>
#'   count(continent) |>
#'   pull(n) |>
#'   max()
#'
#' d |>
#'   mutate(country = forcats::fct_rev(country)) |>
#'   ggplot(aes(y = country, x = value)) +
#'   geom_col(
#'     width = get_width(n = max_n, orientation = "y"),
#'     colour = "black",
#'     fill = "grey",
#'   ) +
#'   facet_wrap(~continent, scales = "free_y") +
#'   scale_y_discrete(continuous.limits = c(1, max_n)) +
#'   coord_cartesian(reverse = "y", clip = "off")
#'
#' mpg |>
#'   ggplot(aes(x = drv)) +
#'   geom_bar(
#'     width = get_width(n = 3, panel_widths = unit(160, "mm")),
#'     colour = "black",
#'     fill = "grey",
#'   ) +
#'   theme(panel.widths = unit(160, "mm"))
#'
get_width <- function(
    ...,
    n = NULL,
    n_dodge = NULL,
    orientation = c("x", "y"),
    equiwidth = NULL,
    panel_widths = NULL,
    panel_heights = NULL
) {
  rlang::check_dots_empty()

  orientation <- rlang::arg_match(orientation)

  if (is.null(n)) {
    rlang::abort("`n` must be specified.", call = rlang::caller_env())
  }
  if (!rlang::is_scalar_integerish(n, finite = TRUE)) {
    rlang::abort("`n` must be a single whole number.", call = rlang::caller_env())
  }
  if (!is.null(n_dodge) && !rlang::is_scalar_integerish(n_dodge, finite = TRUE)) {
    rlang::abort("`n_dodge` must be a single whole number.", call = rlang::caller_env())
  }
  if (!is.null(equiwidth) && (!rlang::is_scalar_double(equiwidth) || !is.finite(equiwidth))) {
    rlang::abort("`equiwidth` must be a single finite numeric value.", call = rlang::caller_env())
  }
  if (!is.null(panel_widths) && !grid::is.unit(panel_widths)) {
    rlang::abort("`panel_widths` must be a `grid::unit` object.", call = rlang::caller_env())
  }
  if (!is.null(panel_heights) && !grid::is.unit(panel_heights)) {
    rlang::abort("`panel_heights` must be a `grid::unit` object.", call = rlang::caller_env())
  }

  # Resolve n_dodge, defaulting to 1 if not supplied
  n_dodge <- n_dodge %||% 1

  # Resolve equiwidth from global option if not supplied, defaulting to 1
  equiwidth <- equiwidth %||% getOption("ggwidth.equiwidth", default = 1)

  # Divide by 7.5 so that equiwidth = 1 is the intuitive default
  equiwidth <- equiwidth / 7.5

  # 1. Get current theme settings, overriding with supplied dims if provided
  current_theme <- ggplot2::theme_get()
  panel_widths  <- panel_widths  %||% current_theme$panel.widths
  panel_heights <- panel_heights %||% current_theme$panel.heights

  # 2. Validate that all panel dimension elements are equal
  check_units_equal(panel_widths,  "panel_widths")
  check_units_equal(panel_heights, "panel_heights")

  # 3. Validation and Conversion
  current_panel_dim <- if (orientation == "x") panel_widths else panel_heights
  current_panel_mm  <- safe_convert_mm(current_panel_dim)

  panel_widths_mm  <- safe_convert_mm(panel_widths)
  panel_heights_mm <- safe_convert_mm(panel_heights)

  if (is.na(panel_widths_mm) || is.na(panel_heights_mm)) {
    rlang::abort(
      "Physical panel widths and heights must both be set in the theme.",
      call = rlang::caller_env()
    )
  }

  # 4. Reference panel dimensions
  ref_panel_widths  <- rep(grid::unit(75, "mm"), 2)
  ref_panel_heights <- rep(grid::unit(50, "mm"), 2)
  ref_panel_dim <- if (orientation == "x") ref_panel_widths else ref_panel_heights
  ref_panel_mm  <- safe_convert_mm(ref_panel_dim)

  # 5. Reference n equiwidth to orientation
  ref_n_x     <- 3
  ref_n_dodge <- 1
  ref_n <- if (orientation == "x") {
    ref_n_x
  } else {
    ref_n_x *
      (safe_convert_mm(ref_panel_heights) / safe_convert_mm(ref_panel_widths))
  }

  # 6. Calculation
  base_width <- (n / ref_n) * equiwidth
  width <- base_width * (n_dodge / ref_n_dodge)

  # 7. Apply Physical Scaling
  if (!is.na(current_panel_mm) && !is.na(ref_panel_mm)) {
    scaling_factor <- ref_panel_mm / current_panel_mm
    width <- width * scaling_factor
  }

  if (width >= 1) {
    rlang::abort(
      "The calculated width must be less than 1. Reduce 'equiwidth' or adjust panel dimensions.",
      call = rlang::caller_env()
    )
  }

  return(width)
}
