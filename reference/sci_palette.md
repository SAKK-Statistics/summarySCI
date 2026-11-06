# SCI color palette

The default colors of all plotting functions of the package.

## Usage

``` r
sci_palette(n, lines = FALSE, bars = FALSE)
```

## Arguments

- n:

  Number of colors.

- lines:

  Logical. If `TRUE`, only the darker shades are used, since the light
  ones are hard to see as lines and points on a white background.
  Default is `FALSE`, for filled boxes.

- bars:

  Logical. If `TRUE`, the palette of the stacked bars: the lightest
  shade is left out, as it is hard to tell from the white background.
  Default is `FALSE`.

## Value

A character vector of `n` hex colors, light to dark.
