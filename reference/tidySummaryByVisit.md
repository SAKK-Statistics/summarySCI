# Read the numbers off a summaryByVisit table

Converts a table produced by
[`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
into a data frame with one row per variable, visit, group and (for
categorical variables) level, holding the parsed numbers. This is the
parser behind
[`plotByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/plotByVisit.md).

## Usage

``` r
tidySummaryByVisit(tbl, stat_cont = NULL)
```

## Arguments

- tbl:

  A table returned by
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
  with `as_flex_table = FALSE` (the default `TRUE` returns a flextable,
  from which the numbers cannot be read back).

- stat_cont:

  The statistic of continuous variables, as passed to
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md).
  Default is `NULL`, which reads it from the table footnote; if the
  footnote is not recognised,
  [`plotByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/plotByVisit.md)
  stops and asks for `stat_cont` rather than guess. A `stat_cont` that
  does not fit the printed cells (e.g. `"mean_sd"` for a median (range)
  table) is an error too.

## Value

A data frame with columns `variable` (the label of the header row),
`var_type`, `visitgroup`, `visit`, `visit_id` (position of the visit
within the variable), `group`, `level` (`NA` for continuous variables),
`stat` (the cell as printed), `n` (the number of non-missing
observations shown in the table's N column), and the parsed numbers
`centre`, `lower`, `upper` (continuous) and `count` (categorical).
