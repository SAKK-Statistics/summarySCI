# Geometric mean

This function return the geometric mean. It is only defined for positive
values: if `x` contains zeros or negative values, `NA` is returned with
a warning (instead of silently dropping them, which
[`log()`](https://rdrr.io/r/base/Log.html) and `na.rm` would otherwise
do).

## Usage

``` r
geom_mean(x, na.rm = TRUE)
```

## Arguments

- x:

  Numeric vector

- na.rm:

  Logical; whether missing values are removed first.

## Value

Geometric mean
