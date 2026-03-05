#' Set a global equiwidth
#'
#' @description
#' Sets a global default for the `equiwidth` argument in `get_width()`.
#' This is the scaling factor that controls the width appearance.
#' All subsequent calls to `get_width()` use this value when
#' `equiwidth = NULL`, and fall back to 1.
#'
#' @param equiwidth Numeric. Multiplicative factor that controls the width appearance.
#'   A value of `1` is the default. Increase to make a wider appearance, and
#'   decrease to make a thinner appearance.
#'
#' @return No return value. Side effects only.
#'
#' @export
#'
#' @seealso [get_width()]
#'
#' @examples
#' set_equiwidth(1)
#' set_equiwidth(0.75)
#' set_equiwidth(1.33)
set_equiwidth <- function(equiwidth = 1) {
  if (!is.numeric(equiwidth) || length(equiwidth) != 1 || !is.finite(equiwidth)) {
    rlang::abort("`equiwidth` must be a single finite numeric value.", call = rlang::caller_env())
  }
  options(ggwidth.equiwidth = equiwidth)
}
