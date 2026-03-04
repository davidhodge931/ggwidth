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
  # Silently return NA if conversion fails — caller is responsible for
  # checking the result and raising an informative error if needed
  tryCatch(
    {
      val <- grid::convertUnit(u, "mm", valueOnly = TRUE)
      if (length(val) > 1) val[1] else val
    },
    error = function(e) NA
  )
}

#' Check that all elements of a grid unit vector are equal
#'
#' @param u A `grid::unit` object, a list containing one, or `NULL`.
#' @param name A string naming the argument, used in the error message.
#'
#' @return No return value. Called for side effects only.
#'
#' @noRd
check_units_equal <- function(u, name) {
  if (!is.null(u) && length(u) > 1) {
    vals <- vapply(
      seq_along(u),
      function(i) grid::convertUnit(u[i], "mm", valueOnly = TRUE),
      numeric(1)
    )
    if (!all(vals == vals[1])) {
      rlang::abort(
        sprintf("All elements of `%s` must be equal.", name),
        call = rlang::caller_env()
      )
    }
  }
}
