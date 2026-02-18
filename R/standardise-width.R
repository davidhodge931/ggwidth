#' Standardise width to the reference
#'
#' @param n Number of categories in the plot.
#' @param dodge_n Number of dodge groups.
#' @param orientation Orientation ("x" or "y").
#' @param ... Require named arguments.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpalmerpenguins)
#'
#' #' Set global theme and width reference
#' theme_set(theme_grey() +
#'             theme(panel.widths = rep(unit(75, "mm"), 2)) +
#'             theme(panel.heights = rep(unit(50, "mm"), 2)))
#'
#' #' 1. Standard vertical bar chart
#' palmerpenguins::penguins |>
#'   filter(!is.na(sex)) |>
#'   ggplot(aes(x = sex, fill = species)) +
#'   geom_bar(
#'     width = standardise_width(
#'       n = 2,
#'       dodge_n = 1,
#'       orientation = "x"
#'     )
#'   )
#'
#' #' 2. Vertical bar chart with dodging
#' #' Note: dodge_n = 3 matches the 3 species in the fill aesthetic
#' palmerpenguins::penguins |>
#'   filter(!is.na(sex)) |>
#'   mutate(sex = str_to_sentence(sex)) |>
#'   ggplot(aes(x = sex, fill = species)) +
#'   geom_bar(
#'     position = position_dodge(),
#'     width = standardise_width(
#'       n = 2,
#'       dodge_n = 3,
#'       orientation = "x"
#'     )
#'   )
#'
#' #' 3. Horizontal bar chart with dodging
#' #' Uses orientation = "y" to scale against panel height
#' palmerpenguins::penguins |>
#'   tidyr::drop_na(sex) |>
#'   mutate(sex = str_to_sentence(sex)) |>
#'   ggplot(aes(y = sex, fill = species)) +
#'   geom_bar(
#'     position = position_dodge(),
#'     width = standardise_width(
#'       n = 2,
#'       dodge_n = 3,
#'       orientation = "y",
#'     )
#'   )
#'
#' #' 4. Faceted horizontal bars with free scales
#' #' Using a reference n ensures bars stay the same thickness across facets
#' d <- tibble::tibble(
#'   continent = c("Europe", "Europe", "Europe", "Europe", "Europe",
#'                 "South America", "South America"),
#'   country = c("AT", "DE", "DK", "ES", "PK", "TW", "BR"),
#'   value = c(10L, 15L, 20L, 25L, 17L, 13L, 5L)
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
#'     width = standardise_width(
#'       n = max_n,
#'       dodge_n = 1,
#'       orientation = "y"
#'     )
#'   ) +
#'   facet_wrap(~continent, scales = "free_y") +
#'   scale_y_discrete(continuous.limits = c(1, max_n)) +
#'   coord_cartesian(reverse = "y", clip = "off")
#'
standardise_width <- function(
    n = NULL,
    dodge_n = 1,
    orientation = "x",
    ...
) {
  if (is.null(n)) rlang::abort("n must be specified")

  # 1. Get current theme settings
  curr_theme <- ggplot2::theme_get()
  p_w <- curr_theme$panel.widths
  p_h <- curr_theme$panel.heights

  # 2. Get global reference standard
  ws <- getOption("ggwidth.width_reference", width_reference)

  # 3. Validation and Conversion
  # Determine which dimension to look at based on orientation
  current_dim_unit <- if (orientation == "x") p_w else p_h
  from_mm <- safe_convert_mm(current_dim_unit)

  # Enforce physical units for orientation = "y"
  if (orientation == "y" && is.na(from_mm)) {
    rlang::abort(
      message = c(
        "When orientation = 'y', physical panel heights must be set in the theme.",
        "i" = "Use theme_set(theme_grey() + theme(panel.heights = list(unit(50, 'mm'))))",
        "x" = "Without absolute units, horizontal bar thickness cannot be standardised."
      )
    )
  }

  # Get reference dimension
  ref_dim_unit <- if (ws$orientation == "x") ws$panel_widths else ws$panel_heights
  to_mm <- safe_convert_mm(ref_dim_unit)

  # 4. Calculation Logic
  base_width <- (n / ws$n) * ws$width
  width <- if (dodge_n > 1 || ws$dodge_n > 1) {
    base_width * (dodge_n / ws$dodge_n)
  } else {
    base_width
  }

  # 5. Apply Physical Scaling
  if (!is.na(from_mm) && !is.na(to_mm)) {
    scaling_factor <- to_mm / from_mm
    width <- width * scaling_factor
  }

  if (any(width >= 1)) {
    rlang::abort("The calculated width must be less than 1. Increase 'n' or reduce the reference width.")
  }

  return(width)
}

#' Convert units to mm safely (Internal)
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

#' Default width reference
#' @noRd
width_reference <- list(
  width = 0.2,
  n = 3,
  dodge_n = 1,
  orientation = "x",
  panel_heights = rep(grid::unit(50, "mm"), 2),
  panel_widths = rep(grid::unit(75, "mm"), 2)
)

#' Update width reference
#'
#' @description
#' Update the width reference used by `standardise_width`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param width Width value for the reference standard.
#' @param n Number of categories (excluding dodge groups) in the reference standard.
#' @param dodge_n Number of dodge groups in reference standard.
#' @param orientation orientation of reference standard ("x" or "y").
#' @param panel_heights Panel heights for reference standard.
#' @param panel_widths Panel widths for reference standard.
#'
#' @export
set_width_reference <- function(
    ...,
    width = NULL,
    n = NULL,
    dodge_n = NULL,
    orientation = NULL,
    panel_heights = NULL,
    panel_widths = NULL
) {
  width_reference <- getOption("ggwidth.width_reference", width_reference)

  if (!rlang::is_null(width)) {
    width_reference$width <- width
  }
  if (!rlang::is_null(n)) {
    width_reference$n <- n
  }
  if (!rlang::is_null(dodge_n)) {
    width_reference$dodge_n <- dodge_n
  }
  if (!rlang::is_null(orientation)) {
    width_reference$orientation <- orientation
  }
  if (!rlang::is_null(panel_heights)) {
    width_reference$panel_heights <- panel_heights
  }
  if (!rlang::is_null(panel_widths)) {
    width_reference$panel_widths <- panel_widths
  }

  options(ggwidth.width_reference = width_reference)
}
