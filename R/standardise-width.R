#' Standardise width to a consistent scale
#'
#' @description
#' Get a ggplot2 width for a plot that will appear consistent across plots.
#'
#' Note this function:
#'
#' * requires a set theme with panel widths and heights specified
#' * requires `"x"` orientation plots to have a x discrete scale, and `"y"` orientation plots to have a y discrete scale.
#'
#' @param n Number of categories in the orientation aesthetic (i.e. `"x"` or `"y"`).
#'   For faceted plots, use the maximum `n` within a facet.
#' @param n_dodge Number of dodge categories. Must match the number of groups in
#'   the `fill` or `colour` aesthetic when using `position_dodge()`. If `NA`, then 1.
#' @param orientation Orientation: `"x"` for vertical (width appearance scaled to
#'   panel width), `"y"` for horizontal (width appearance scaled to panel height).
#' @param scale Numeric. Multiplicative factor that controls the width scale.
#'   A value of `1` (default) is the default width scale. Increase to make
#'   a wider width scale, and decrease to make a thinner width scale.
#'   If `NULL`, uses the value set by `set_width()`, falling back to `1`.
#' @param panel_widths A `grid::unit` object specifying the panel width. If `NULL`
#'   (default), uses the value set in the current theme.
#' @param panel_heights A `grid::unit` object specifying the panel height. If `NULL`
#'   (default), uses the value set in the current theme.
#' @param ... Reserved for future use. Requires named arguments.
#'
#' @return A numeric width value passed to the `width` argument of
#'   `geom_bar()`, `geom_col()`, or similar geoms.
#'
#' @export
#'
#' @seealso [set_width()]
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' set_theme(
#'   theme_grey() +
#'     theme(panel.widths  = rep(grid::unit(75, "mm"), 2)) +
#'     theme(panel.heights = rep(grid::unit(50, "mm"), 2))
#' )
#'
#' set_width(scale = 1)
#'
#' # Example 1: 3 species, vertical bars
#' palmerpenguins::penguins |>
#'   filter(!is.na(sex)) |>
#'   ggplot(aes(x = species)) +
#'   geom_bar(width = standardise_width(n = 3))
#'
#' # Example 2: 7 categories, vertical bars
#' diamonds |>
#'   ggplot(aes(x = color)) +
#'   geom_bar(width = standardise_width(n = 7))
#'
#' # Example 3: 7 categories, horizontal bars
#' diamonds |>
#'   ggplot(aes(y = color)) +
#'   geom_bar(width = standardise_width(n = 7, orientation = "y"))
#'
#' # Example 4: Dodged bars, vertical
#' palmerpenguins::penguins |>
#'   filter(!is.na(sex)) |>
#'   ggplot(aes(x = sex, fill = species)) +
#'   geom_bar(
#'     position = position_dodge(),
#'     width = standardise_width(n = 2, n_dodge = 3)
#'   )
#'
#' # Example 5: Dodged bars, horizontal
#' palmerpenguins::penguins |>
#'   tidyr::drop_na(sex) |>
#'   ggplot(aes(y = sex, fill = species)) +
#'   geom_bar(
#'     position = position_dodge(),
#'     width = standardise_width(n = 2, n_dodge = 3, orientation = "y")
#'   )
#'
#' # Example 6: Faceted plot using max n across facets
#' d <- tibble::tibble(
#'   continent = c("Europe", "Europe", "Europe", "Europe", "Europe",
#'                 "South America", "South America"),
#'   country   = c("AT", "DE", "DK", "ES", "PK", "TW", "BR"),
#'   value     = c(10L, 15L, 20L, 25L, 17L, 13L, 5L)
#' )
#' max_n <- d |>
#'   count(continent) |>
#'   pull(n) |>
#'   max()
#' d |>
#'   mutate(country = forcats::fct_rev(country)) |>
#'   ggplot(aes(y = country, x = value)) +
#'   geom_col(width = standardise_width(n = max_n, orientation = "y")) +
#'   facet_wrap(~continent, scales = "free_y") +
#'   scale_y_discrete(continuous.limits = c(1, max_n)) +
#'   coord_cartesian(reverse = "y", clip = "off")
#'
#' # Example 7: Overriding panel_widths for a wider panel
#' palmerpenguins::penguins |>
#'   filter(!is.na(sex)) |>
#'   ggplot(aes(x = species)) +
#'   geom_bar(
#'     width = standardise_width(n = 3, panel_widths = grid::unit(160, "mm"))
#'   ) +
#'   theme(panel.widths = rep(grid::unit(160, "mm"), 2))
#'
standardise_width <- function(
    n = NULL,
    n_dodge = 1,
    orientation = "x",
    scale = NULL,
    panel_widths = NULL,
    panel_heights = NULL,
    ...
) {
  if (is.null(n)) rlang::abort("n must be specified")

  is_unit <- function(x) inherits(x, "unit")

  if (!is.null(panel_widths) && !is_unit(panel_widths))
    rlang::abort("`panel_widths` must be a `grid::unit` object.")
  if (!is.null(panel_heights) && !is_unit(panel_heights))
    rlang::abort("`panel_heights` must be a `grid::unit` object.")

  # Resolve scale from global option if not supplied, defaulting to 1
  scale <- scale %||% getOption("ggwidth.scale", default = 1)

  # Convert to internal width
  scale <- scale / 7.5

  # 1. Get current theme settings, overriding with supplied dims if provided
  current_theme <- ggplot2::theme_get()
  panel_widths  <- panel_widths  %||% current_theme$panel.widths
  panel_heights <- panel_heights %||% current_theme$panel.heights

  # 2. Validation and Conversion
  current_panel_dim <- if (orientation == "x") panel_widths else panel_heights
  current_panel_mm  <- safe_convert_mm(current_panel_dim)

  panel_widths_mm  <- safe_convert_mm(panel_widths)
  panel_heights_mm <- safe_convert_mm(panel_heights)

  if (is.na(panel_widths_mm) || is.na(panel_heights_mm)) {
    rlang::abort(
      message = c("Physical panel widths and heights must both be set in the theme.")
    )
  }

  # 3. Reference panel dimensions
  ref_panel_widths  <- rep(grid::unit(75, "mm"), 2)
  ref_panel_heights <- rep(grid::unit(50, "mm"), 2)
  ref_panel_dim     <- if (orientation == "x") ref_panel_widths else ref_panel_heights
  ref_panel_mm      <- safe_convert_mm(ref_panel_dim)

  # 4. Reference n scaled to orientation
  ref_n_x     <- 3
  ref_n_dodge <- 1
  ref_n <- if (orientation == "x") {
    ref_n_x
  } else {
    ref_n_x * (safe_convert_mm(ref_panel_heights) / safe_convert_mm(ref_panel_widths))
  }

  # 5. Calculation
  base_width <- (n / ref_n) * scale
  width <- if (n_dodge > 1 || ref_n_dodge > 1) {
    base_width * (n_dodge / ref_n_dodge)
  } else {
    base_width
  }

  # 6. Apply Physical Scaling
  if (!is.na(current_panel_mm) && !is.na(ref_panel_mm)) {
    scaling_factor <- ref_panel_mm / current_panel_mm
    width <- width * scaling_factor
  }

  if (any(width >= 1)) {
    rlang::abort("The calculated width must be less than 1. Reduce 'scale' or adjust panel dimensions.")
  }

  return(width)
}

#' Convert units to mm safely
#' @noRd
safe_convert_mm <- function(x) {
  if (is.null(x)) return(NA)

  u <- if (is.list(x)) x[[1]] else x[1]

  tryCatch({
    val <- grid::convertUnit(u, "mm", valueOnly = TRUE)
    if (length(val) > 1) val[1] else val
  }, error = function(e) NA)
}

#' Set a global width scale
#'
#' @description
#' Sets a global default for the `scale` argument in `standardise_width()`.
#' All subsequent calls to `standardise_width()` will use this value when
#' `scale = NULL`.
#'
#' @param scale Numeric. A value of `1` (default) corresponds to a default
#'   width scale. Increase to make a wider width scale, and decrease
#'   to make a thinner width scale.
#'
#' @seealso [standardise_width()]
#'
#' @export
#'
#' @examples
#' set_width(1)
#' set_width(0.75)
#' set_width(1.33)
set_width <- function(scale = 1) {
  options(ggwidth.scale = scale)
}
