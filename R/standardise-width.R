#' Standardise bar width across plots
#'
#' @description
#' Calculates a ggplot2 bar width that produces consistent physical bar
#' thickness across plots, orientations, and panel sizes. Set `scale` to
#' the same value across all calls to ensure bars are visually comparable.
#'
#' @param n Number of categories in the plot. For faceted plots, use the
#'   maximum `n` across all facets to keep bar thickness consistent.
#' @param dodge_n Number of dodge groups. Must match the number of levels in
#'   the `fill` or `colour` aesthetic when using `position_dodge()`.
#' @param orientation Orientation of the bars: `"x"` for vertical bars
#'   (thickness scaled to panel width), `"y"` for horizontal bars (thickness
#'   scaled to panel height). Panel heights must be set in absolute units when
#'   using `"y"`.
#' @param scale Bar width scale factor. A value of `1` (default) corresponds
#'   to a standard bar width. Increase to make bars thicker, decrease to make
#'   them thinner. Use the same value across plots for consistent bar thickness.
#' @param ... Reserved for future use. Requires named arguments.
#'
#' @return A numeric width value passed to the `width` argument of
#'   `geom_bar()` or `geom_col()`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # Set global theme and panel dimensions
#' theme_set(
#'   theme_grey() +
#'     theme(panel.widths  = rep(unit(50, "mm"), 2)) +
#'     theme(panel.heights = rep(unit(75, "mm"), 2))
#' )
#'
#' # 1. Standard vertical bar chart
#' palmerpenguins::penguins |>
#'   filter(!is.na(sex)) |>
#'   ggplot(aes(x = species, fill = species)) +
#'   geom_bar(
#'     width = standardise_width(n = 3, dodge_n = 1, orientation = "x")
#'   )
#'
#' # 2. Vertical bar chart with dodging
#' # dodge_n = 3 matches the 3 species in the fill aesthetic
#' palmerpenguins::penguins |>
#'   filter(!is.na(sex)) |>
#'   ggplot(aes(x = sex, fill = species)) +
#'   geom_bar(
#'     position = position_dodge(),
#'     width = standardise_width(n = 2, dodge_n = 3, orientation = "x")
#'   )
#'
#' # 3. Horizontal bar chart with dodging
#' # orientation = "y" scales thickness against panel height
#' palmerpenguins::penguins |>
#'   tidyr::drop_na(sex) |>
#'   ggplot(aes(y = sex, fill = species)) +
#'   geom_bar(
#'     position = position_dodge(),
#'     width = standardise_width(n = 2, dodge_n = 3, orientation = "y")
#'   )
#'
#' # 4. Faceted horizontal bars with free scales
#' # Using max_n ensures bar thickness is consistent across facets
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
#'     width = standardise_width(n = max_n, dodge_n = 1, orientation = "y")
#'   ) +
#'   facet_wrap(~continent, scales = "free_y") +
#'   scale_y_discrete(continuous.limits = c(1, max_n)) +
#'   coord_cartesian(reverse = "y", clip = "off")
standardise_width <- function(
    n = NULL,
    dodge_n = 1,
    orientation = "x",
    scale = 1,
    ...
) {
  if (is.null(n)) rlang::abort("n must be specified")

  # Convert scale to standard
  standard <- scale / 5

  # 1. Get current theme settings
  current_theme  <- ggplot2::theme_get()
  panel_widths   <- current_theme$panel.widths
  panel_heights  <- current_theme$panel.heights

  # 2. Validation and Conversion
  current_panel_dim <- if (orientation == "x") panel_widths else panel_heights
  current_panel_mm  <- safe_convert_mm(current_panel_dim)

  if (orientation == "y" && is.na(current_panel_mm)) {
    rlang::abort(
      message = c(
        "When orientation = 'y', physical panel heights must be set in the theme.",
        "i" = "Use theme_set(theme_grey() + theme(panel.heights = list(unit(50, 'mm'))))",
        "x" = "Without absolute units, horizontal bar thickness cannot be standardised."
      )
    )
  }

  # 3. Reference panel dimensions
  ref_panel_widths  <- rep(grid::unit(75, "mm"), 2)
  ref_panel_heights <- rep(grid::unit(50, "mm"), 2)
  ref_panel_dim     <- if (orientation == "x") ref_panel_widths else ref_panel_heights
  ref_panel_mm      <- safe_convert_mm(ref_panel_dim)

  # 4. Reference n scaled to orientation so that scale = 1 produces
  #    equivalent physical bar thickness regardless of orientation
  ref_n_x     <- 3
  ref_dodge_n <- 1
  ref_n <- if (orientation == "x") {
    ref_n_x
  } else {
    ref_n_x * (safe_convert_mm(ref_panel_heights) / safe_convert_mm(ref_panel_widths))
  }

  # 5. Calculation
  base_width <- (n / ref_n) * standard
  width <- if (dodge_n > 1 || ref_dodge_n > 1) {
    base_width * (dodge_n / ref_dodge_n)
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

  # Handle list (theme) or vector (unit object) by taking the first element
  u <- if (is.list(x)) x[[1]] else x[1]

  tryCatch({
    # Perform conversion
    val <- grid::convertUnit(u, "mm", valueOnly = TRUE)
    # Ensure a single numeric value comes back
    if (length(val) > 1) val[1] else val
  }, error = function(e) NA)
}
