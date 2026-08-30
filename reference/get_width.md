# Standardise `ggplot2` geom width

Standardise the width in `ggplot2` geoms so that widths appear visually
consistent across plots with different numbers of categories, panel
dimensions, and orientations.

This can be used in geoms such as
[`ggplot2::geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
[`ggplot2::geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
[`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html),
and
[`ggplot2::geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html).

The relevant panel dimension must be an absolute physical unit (for
example `grid::unit(..., "mm")`), either supplied directly or set in the
current theme. Relative units are not supported.

## Usage

``` r
get_width(
  ...,
  n = NULL,
  n_dodge = NULL,
  orientation = c("x", "y"),
  equiwidth = NULL,
  panel_widths = NULL,
  panel_heights = NULL
)
```

## Arguments

- ...:

  Must be empty. Forces all other arguments to be named and allows
  trailing commas.

- n:

  Number of categories in the orientation aesthetic (that is, `"x"` or
  `"y"`). For faceted plots, use the maximum `n` across facets.

- n_dodge:

  Number of dodge categories. Intended for use with
  `position_dodge(preserve = "single")`.

- orientation:

  Orientation: `"x"` for vertical geoms, `"y"` for horizontal geoms.

- equiwidth:

  Numeric scaling factor controlling apparent width. A value of `1` is
  the default. Increase to make a wider appearance; decrease to make a
  thinner appearance. If `NULL`, uses the value set by
  [`update_equiwidth()`](https://davidhodge931.github.io/ggwidth/reference/update_equiwidth.md),
  falling back to `1`.

- panel_widths:

  A [`grid::unit`](https://rdrr.io/r/grid/unit.html) object specifying
  panel widths. If `NULL`, uses the value from the current theme.

- panel_heights:

  A [`grid::unit`](https://rdrr.io/r/grid/unit.html) object specifying
  panel heights. If `NULL`, uses the value from the current theme.

## Value

A single numeric width suitable for the `width` argument of geoms such
as
[`ggplot2::geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)
or
[`ggplot2::geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html).
