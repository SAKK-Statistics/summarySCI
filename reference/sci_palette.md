# SCI color palette

The default colors of all plotting functions of the package.

## Usage

``` r
sci_palette(n, lines = FALSE)
```

## Arguments

- n:

  Number of colors.

- lines:

  Logical. If `TRUE`, only the darker shades are used, since the light
  ones are hard to see as lines and points on a white background.
  Default is `FALSE`, for filled boxes and bars.

## Value

A character vector of `n` hex colors, light to dark.
