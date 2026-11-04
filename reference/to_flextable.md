# Convert a gtsummary table to a flextable

The conversion
[`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md)
applies when `as_flex_table = TRUE`: a flextable fitted to the page
width, with an outer border if requested.

## Usage

``` r
to_flextable(tbl, border = TRUE)
```

## Arguments

- tbl:

  A gtsummary table.

- border:

  Logical. Draw a border around header and body.

## Value

A flextable.
