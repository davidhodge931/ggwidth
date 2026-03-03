#' Default value for NULL
#'
#' @description
#' Returns `x` if it is not `NULL`, otherwise returns `y`.
#'
#' @param x An object.
#' @param y A default value to use if `x` is `NULL`.
#'
#' @return `x` if not `NULL`, otherwise `y`.
#'
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Convert a grid unit to millimetres safely
#'
#' @param x A `grid::unit` object, a list containing one, or `NULL`.
#'
#' @return A single numeric value in millimetres, or `NA` if conversion fails
#'   or `x` is `NULL`.
#'
#' @noRd
safe_convert_mm <- function(x) {
  if (is.null(x)) {
    return(NA)
  }
  u <- if (is.list(x)) x[[1]] else x[1]
  tryCatch(
    {
      val <- grid::convertUnit(u, "mm", valueOnly = TRUE)
      if (length(val) > 1) val[1] else val
    },
    error = function(e) NA
  )
}
