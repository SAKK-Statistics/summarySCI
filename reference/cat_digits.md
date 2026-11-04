# Digits for the categorical statistics

Counts (n, N) are shown without decimals; `digits_cat` is used for the
percentages only. The digits are returned in the order of the statistics
in `stat_fmt`, as gtsummary expects.

## Usage

``` r
cat_digits(stat_fmt, digits_cat)
```

## Arguments

- stat_fmt:

  Statistic string, e.g. `"{n} ({p}%)"`.

- digits_cat:

  Digits for percentages.

## Value

Integer vector, one value per statistic.
