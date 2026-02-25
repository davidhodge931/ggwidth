#' Set a global equiwidth
#'
#' @description
#' Sets a global default for the `equiwidth` argument in `get_width()`.
#' This is the scaling factor that controls the width appearance.
#' All subsequent calls to `get_width()` will use this value when
#' `equiwidth = NULL` falling back to 1.
#'
#' @param equiwidth Numeric. Multiplicative factor that controls the width appearance.
#'   A value of `1` (default) is the default. Increase to make a wider appearance, and
#'   decrease to make a thinner appearance. If `NULL`, uses the value set by `set_equiwidth()`,
#'   falling back to `1`.
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
  options(ggwidth.equiwidth = equiwidth)
}
