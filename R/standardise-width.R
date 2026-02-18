#' Standardise width to a consistent appearance
#'
#' @description
#' Get a ggplot2 width for a plot that will appear consistent across plots.
#'
#' @param n Number of categories in the orientation aesthetic (i.e. `"x"` or `"y"`).
#'   For faceted plots, use the maximum `n` within a facet.
#' @param n_dodge Number of dodge categories. Must match the number of groups in
#'   the `fill` or `colour` aesthetic when using `position_dodge()`. If `NA`, then 1.
#' @param orientation Orientation: `"x"` for vertical (width appearance scaled to
#'   panel width), `"y"` for horizontal (width appearance scaled to panel height).
#' @param appearance Numeric. Multiplicative factor that controls the width appearance.
#'   A value of `1` (default) is the default width appearance. Increase to make
#'   a wider width appearance, and decrease to make a thinner width appearance.
#'   If `NULL`, uses the value set by `set_width()`, falling back to `1`.
#' @param ... Reserved for future use. Requires named arguments.
#'
#' @return A numeric width value passed to the `width` argument of
#'   `geom_bar()`, `geom_col()`, or similar geoms.
#'
#' @export
#'
#' @seealso [set_width()]
#'
standardise_width <- function(
    n = NULL,
    n_dodge = 1,
    orientation = "x",
    appearance = NULL,
    ...
) {
  if (is.null(n)) rlang::abort("n must be specified")

  # Resolve appearance from global option if not supplied, defaulting to 1
  appearance <- appearance %||% getOption("ggwidth.appearance", default = 1)

  # Convert to internal width
  appearance <- appearance / 7.5

  # 1. Get current theme settings
  current_theme  <- ggplot2::theme_get()
  panel_widths   <- current_theme$panel.widths
  panel_heights  <- current_theme$panel.heights

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
  base_width <- (n / ref_n) * appearance
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
    rlang::abort("The calculated width must be less than 1. Reduce 'appearance' or adjust panel dimensions.")
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

#' Set a global width appearance
#'
#' @description
#' Sets a global default for the `appearance` argument in `standardise_width()`.
#' All subsequent calls to `standardise_width()` will use this value when
#' `appearance = NULL`.
#'
#' @param appearance Numeric. A value of `1` (default) corresponds to a default
#'   width appearance. Increase to make a wider width appearance, and decrease
#'   to make a thinner width appearance.
#'
#' @seealso [standardise_width()]
#'
#' @export
#'
#' @examples
#' set_width(1)
#' set_width(0.75)
#' set_width(1.33)
set_width <- function(appearance = 1) {
  options(ggwidth.appearance = appearance)
}
