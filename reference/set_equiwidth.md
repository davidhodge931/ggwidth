# Set a global equiwidth

Sets a global default for the `equiwidth` argument in
[`get_width()`](https://davidhodge931.github.io/ggwidth/reference/get_width.md).
This is the scaling factor that controls the width appearance. All
subsequent calls to
[`get_width()`](https://davidhodge931.github.io/ggwidth/reference/get_width.md)
will use this value when `equiwidth = NULL` falling back to 1.

## Usage

``` r
set_equiwidth(equiwidth = 1)
```

## Arguments

- equiwidth:

  Numeric. Multiplicative factor that controls the width appearance. A
  value of `1` (default) is the default. Increase to make a wider
  appearance, and decrease to make a thinner appearance. If `NULL`, uses
  the value set by `set_equiwidth()`, falling back to `1`.

## Value

No return value. Side effects only.

## See also

[`get_width()`](https://davidhodge931.github.io/ggwidth/reference/get_width.md)

## Examples

``` r
set_equiwidth(1)
set_equiwidth(0.75)
set_equiwidth(1.33)
```
