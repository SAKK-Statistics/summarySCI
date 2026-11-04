# One row of patient numbers beneath the x-axis

Draws the numbers `n` at the x positions `at` on margin line `line`,
zero counts shown as `missing_nr`, and optionally the row label
`"# <label>"` left of the plot, as in
[`boxplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/boxplotSCI.md).

## Usage

``` r
risk_row(n, line, at, label = NULL, label_at = NULL, cex = 0.7, missing_nr = 0)
```

## Arguments

- n:

  Numbers of patients.

- line:

  Margin line of the row.

- at:

  x positions of the numbers.

- label:

  Group label of the row, or `NULL` for none.

- label_at:

  x position of the right end of the label.

- cex:

  Font size.

- missing_nr:

  Shown instead of zero counts.
