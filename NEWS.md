# summarySCI 1.0.0

## New functions

- `summarizeSCI()` and `summarizeByVisitSCI()`: the table of `summaryTable()`
  / `summaryByVisit()` together with one plot per variable that matches it.
- `boxplotSCI()`, `lineplotSCI()` and `stackedbarplotSCI()`: plots over time
  with a common layout, optional patient numbers and statistic footnote.
- `plotByVisit()` and `tidySummaryByVisit()`: plot or extract the numbers of a
  `summaryByVisit()` table.
- `save_plot()` and `save_plots()`: write plots to files.
- Example data `hemoglobin_data` and new vignettes.

## New functionality

- `summaryByVisit()` supports categorical variables.
- `summaryTable()` supports any number of groups, and has new arguments
  `continuous_as_categorical`, `dichotomous_as_categorical` and `ref_level`.
- `summaryLevels()` has a new argument `add_n`.

## Breaking changes

- `file_name` is now `file_path`.
- `ci_cat` defaults to `"clopper.pearson"` (was `"wilson"`).
- `add_n` defaults to `TRUE` in `summaryByVisit()`.
- `test_cont` defaults to Wilcoxon for two groups and Kruskal-Wallis for more.
- `stat_cont = "geomMean_sd"` shows the geometric SD (was the arithmetic SD).

## Other changes

- Character variables and visits are sorted naturally ("2" before "10").
- `digits_cat` applies to percentages only; counts have no decimals.
- More informative error messages, extensive tests and bug fixes.

# summarySCI 0.1.2

Removed the deprecated options from `gtsummary`

# summarySCI 0.1.0

Name changed from `summarySAKK` to `summarySCI`.
