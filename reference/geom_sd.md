# Geometric standard deviation

The standard deviation on the log scale, back-transformed:
`exp(sd(log(x)))`. It is a factor: about two thirds of log-normal values
lie between the geometric mean divided and multiplied by it. Like
[`geom_mean()`](https://sakk-statistics.github.io/summarySCI/reference/geom_mean.md),
it is only defined for positive values and is `NA` otherwise (without a
second warning), and `NA` for fewer than two values.

## Usage

``` r
geom_sd(x, na.rm = TRUE)
```

## Arguments

- x:

  Numeric vector

- na.rm:

  Logical; whether missing values are removed first.

## Value

Geometric standard deviation
