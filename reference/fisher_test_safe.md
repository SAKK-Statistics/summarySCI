# Robust Fisher's exact test for gtsummary::add_p()

Runs stats::fisher.test(); if it fails because the FEXACT workspace is
too small (tables with many levels), retries with a larger workspace and
finally falls back to a Monte Carlo p-value (reproducible with
[`set.seed()`](https://rdrr.io/r/base/Random.html)). Returns NA if the
table is degenerate.

## Usage

``` r
fisher_test_safe(data, variable, by, ...)
```

## Arguments

- data, variable, by:

  Passed by gtsummary::add_p().

## Value

A data frame with columns `p.value` and `method`.
