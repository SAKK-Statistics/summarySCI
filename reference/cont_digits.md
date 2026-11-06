# Digits for the continuous statistics

`digits_cont` for every statistic, except the geometric SD: a factor
close to 1 (e.g. 1.13), which gets at least 2 decimals. The digits are
returned in the order of the statistics in `stat_fmt`, as gtsummary
expects.

## Usage

``` r
cont_digits(stat_fmt, digits_cont)
```

## Arguments

- stat_fmt:

  Statistic string, e.g. `"{geom_mean} ({geom_sd})"`.

- digits_cont:

  Digits for continuous statistics.

## Value

Integer vector, one value per statistic.
