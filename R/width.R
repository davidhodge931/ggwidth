#' Standardise `ggplot2` geom width
#'
#' @description
#' Standardise the width in `ggplot2` geoms so that widths appear visually
#' consistent across plots with different numbers of categories, panel dimensions,
#' and orientations.
#'
#' This can be used in geoms such as [ggplot2::geom_bar()],
#' [ggplot2::geom_col()], [ggplot2::geom_boxplot()],
#' and [ggplot2::geom_errorbar()].
#'
#' The relevant panel dimension must be an absolute physical unit (for example
#' `grid::unit(..., "mm")`), either supplied directly or set in the current theme.
#' Relative units are not supported.
#'
#' @param ... Must be empty. Forces all other arguments to be named and allows
#'   trailing commas.
#' @param n Number of categories in the orientation aesthetic (that is, `"x"` or
#'   `"y"`). For faceted plots, use the maximum `n` across facets.
#' @param n_dodge Number of dodge categories. Intended for use with
#'   `position_dodge(preserve = "single")`.
#' @param orientation Orientation: `"x"` for vertical geoms, `"y"` for horizontal geoms.
#' @param equiwidth Numeric scaling factor controlling apparent width. A value of
#'   `1` is the default. Increase to make a wider appearance; decrease to make a
#'   thinner appearance. If `NULL`, uses the value set by [update_equiwidth()],
#'   falling back to `1`.
#' @param panel_widths A `grid::unit` object specifying panel widths. If `NULL`,
#'   uses the value from the current theme.
#' @param panel_heights A `grid::unit` object specifying panel heights. If `NULL`,
#'   uses the value from the current theme.
#'
#' @return A single numeric width suitable for the `width` argument of geoms such
#'   as [ggplot2::geom_bar()] or [ggplot2::geom_col()].
#'
#' @export
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

  validate_positive_whole_number(n, "n", required = TRUE)
  validate_positive_whole_number(n_dodge, "n_dodge", required = FALSE)
  validate_positive_number(equiwidth, "equiwidth", required = FALSE)

  n_dodge <- n_dodge %||% 1L
  equiwidth <- equiwidth %||% getOption("ggwidth.equiwidth", default = 1)

  current_theme <- ggplot2::theme_get()
  panel_widths  <- panel_widths  %||% current_theme$panel.widths
  panel_heights <- panel_heights %||% current_theme$panel.heights

  panel_dim <- if (orientation == "x") panel_widths else panel_heights
  panel_arg <- if (orientation == "x") "panel_widths" else "panel_heights"

  check_units_equal(panel_dim, panel_arg)

  panel_mm <- safe_convert_mm(panel_dim)
  if (is.na(panel_mm)) {
    rlang::abort(
      paste0(
        "`", panel_arg, "` must be an absolute physical unit ",
        '(for example `grid::unit(..., "mm")`).'
      )
    )
  }

  # Reference configuration:
  # - x orientation: 3 categories in a 75 mm panel
  # - y orientation: equivalent appearance in a 50 mm panel
  ref_panel_width_mm  <- 75
  ref_panel_height_mm <- 50
  ref_n_x <- 3
  ref_n_y <- ref_n_x * (ref_panel_height_mm / ref_panel_width_mm)  # = 2

  ref_n <- if (orientation == "x") ref_n_x else ref_n_y

  # Normalisation chosen so equiwidth = 1 corresponds to the package's
  # reference visual width under the reference panel dimensions.
  equiwidth_norm <- equiwidth / 5

  width <- (n / ref_n) * equiwidth_norm * n_dodge

  ref_panel_mm <- if (orientation == "x") ref_panel_width_mm else ref_panel_height_mm
  width <- width * (ref_panel_mm / panel_mm)

  if (width >= 1) {
    rlang::abort(
      paste(
        "The calculated width is >= 1.",
        "Reduce `equiwidth`, reduce `n_dodge`, increase the relevant panel dimension,",
        "or reconsider the reference calibration."
      )
    )
  }

  width
}

#' Update the global equiwidth
#'
#' @description
#' Update a global default for the `equiwidth` argument used by [get_width()].
#'
#' @param equiwidth A single positive finite numeric value.
#'
#' @return The previous option value, invisibly.
#'
#' @export
update_equiwidth <- function(equiwidth = 1) {
  validate_positive_number(equiwidth, "equiwidth", required = TRUE)

  old <- getOption("ggwidth.equiwidth", default = 1)
  options(ggwidth.equiwidth = equiwidth)
  invisible(old)
}

#' Convert a grid unit to millimetres safely
#'
#' @param x A `grid::unit` object, a list containing one, or `NULL`.
#'
#' @return A single numeric value in millimetres, or `NA_real_` if conversion
#'   fails or `x` is `NULL`.
#'
#' @noRd
safe_convert_mm <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }

  u <- if (is.list(x)) x[[1]] else x[1]

  tryCatch(
    grid::convertUnit(u, "mm", valueOnly = TRUE),
    error = function(e) NA_real_
  )
}

#' Check that all elements of a grid unit vector are equal
#'
#' @param unit A `grid::unit` object or `NULL`.
#' @param name Name of the argument, used in error messages.
#'
#' @return Called for side effects only.
#'
#' @noRd
check_units_equal <- function(unit, name) {
  if (is.null(unit) || length(unit) <= 1) {
    return(invisible())
  }

  vals <- vapply(seq_along(unit), function(i) safe_convert_mm(unit[i]), numeric(1))

  if (anyNA(vals)) {
    rlang::abort(
      paste0(
        "`", name, "` must contain absolute physical units only ",
        '(for example `grid::unit(..., "mm")`).'
      ),
      call = rlang::caller_env()
    )
  }

  if (!isTRUE(all.equal(vals, rep(vals[1], length(vals))))) {
    rlang::abort(
      paste0("All elements of `", name, "` must be equal."),
      call = rlang::caller_env()
    )
  }

  invisible()
}

#' Validate a positive whole number
#'
#' @noRd
validate_positive_whole_number <- function(x, name, required = TRUE) {
  if (is.null(x)) {
    if (required) {
      rlang::abort(paste0("`", name, "` must be specified."))
    }
    return(invisible())
  }

  if (!rlang::is_scalar_integerish(x, finite = TRUE) || x <= 0) {
    rlang::abort(paste0("`", name, "` must be a single positive whole number."))
  }

  invisible()
}

#' Validate a positive finite number
#'
#' @noRd
validate_positive_number <- function(x, name, required = TRUE) {
  if (is.null(x)) {
    if (required) {
      rlang::abort(paste0("`", name, "` must be specified."))
    }
    return(invisible())
  }

  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x <= 0) {
    rlang::abort(paste0("`", name, "` must be a single positive finite numeric value."))
  }

  invisible()
}
