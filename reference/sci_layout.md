# Layout of the plots

Margins, lines of the patient numbers and of the x-axis title shared by
[`boxplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/boxplotSCI.md),
[`lineplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/lineplotSCI.md)
and
[`stackedbarplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/stackedbarplotSCI.md),
so that plots used together line up. The blueprint is the layout of
[`boxplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/boxplotSCI.md).

## Usage

``` r
sci_layout(n_time, n_groups, risk_table, offset = 0, footnote = FALSE)
```

## Arguments

- n_time:

  Number of time points; `1` for a single time point.

- n_groups:

  Number of groups.

- risk_table:

  Logical. Are patient numbers drawn beneath the x-axis?

- offset:

  Lines taken by extra labels beneath the x-axis (e.g. group labels
  under bars, or rotated labels), by which the numbers and the x-axis
  title move down.

- footnote:

  Logical. Is a footnote on the statistic drawn beneath the x-axis
  title? It adds the same space to every plot type.

## Value

A list with `mar` (for `par(mar = )`), `rows` (margin lines of the rows
of patient numbers, one per group over time, one for a single time
point), `xlab_line` and `note_line` (the line of the footnote).
