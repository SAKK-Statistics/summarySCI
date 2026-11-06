# Changelog

## summarySCI 1.0.0

### New functions

- [`summarizeSCI()`](https://sakk-statistics.github.io/summarySCI/reference/summarizeSCI.md)
  and
  [`summarizeByVisitSCI()`](https://sakk-statistics.github.io/summarySCI/reference/summarizeByVisitSCI.md):
  the table of
  [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md)
  /
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
  together with one plot per variable that matches it.
- [`boxplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/boxplotSCI.md),
  [`lineplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/lineplotSCI.md)
  and
  [`stackedbarplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/stackedbarplotSCI.md):
  plots over time with a common layout, optional patient numbers and
  statistic footnote.
- [`plotByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/plotByVisit.md)
  and
  [`tidySummaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/tidySummaryByVisit.md):
  plot or extract the numbers of a
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
  table.
- [`save_plot()`](https://sakk-statistics.github.io/summarySCI/reference/save_plot.md)
  and
  [`save_plots()`](https://sakk-statistics.github.io/summarySCI/reference/save_plots.md):
  write plots to files.
- Example data `hemoglobin_data` and new vignettes.

### New functionality

- [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
  supports categorical variables.
- [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md)
  supports any number of groups, and has new arguments
  `continuous_as_categorical`, `dichotomous_as_categorical` and
  `ref_level`.
- [`summaryLevels()`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md)
  has a new argument `add_n`.

### Breaking changes

- `file_name` is now `file_path`.
- `ci_cat` defaults to `"clopper.pearson"` (was `"wilson"`).
- `add_n` defaults to `TRUE` in
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md).
- `test_cont` defaults to Wilcoxon for two groups and Kruskal-Wallis for
  more.
- `stat_cont = "geomMean_sd"` shows the geometric SD (was the arithmetic
  SD).

### Other changes

- Character variables and visits are sorted naturally (“2” before “10”).
- `digits_cat` applies to percentages only; counts have no decimals.
- More informative error messages, extensive tests and bug fixes.

## summarySCI 0.1.2

CRAN release: 2026-09-14

Removed the deprecated options from `gtsummary`

## summarySCI 0.1.0

Name changed from `summarySAKK` to `summarySCI`.
