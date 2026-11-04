# Changelog

## summarySCI 1.0.0

- Improvement of the internal structure of the function

- More informative error messages

- No more upper limit for the number of groups in
  [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md).

- [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
  now also accepts categorical variables.

- Argument `file_name` was changed to `file_path`

- New function
  [`boxplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/boxplotSCI.md)

- Intensive tests have been added

- The default of `digits_cat` in
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
  was changed from 1 to 0, consistent with
  [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md)
  and
  [`summaryLevels()`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md).

- The default of `add_n` in
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
  was changed from FALSE to TRUE, consistent with
  [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md).

- In
  [`summaryLevels()`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md),
  the Overall column is now shown at the end of the table (with the
  p-value before it), consistent with the other functions.

- [`summaryLevels()`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md)
  now gives an informative error if a variable is not binary (e.g. 0/1,
  yes/no) and `levels` is not given, instead of showing counts of 0.
  Several values in `levels` are now all counted as present, and yes/no
  variables with missing values are handled correctly.

- New argument `add_n` in
  [`summaryLevels()`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md)
  (default TRUE), consistent with
  [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md)
  and
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md).

- The default CI method of
  [`summaryLevels()`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md)
  was changed from Wilson to Clopper-Pearson
  (`ci_cat = "clopper.pearson"`), consistent with
  [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md).
  `ci_cat = "clopper.pearson"` failed before in
  [`summaryLevels()`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md)
  and now works.

- In
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md),
  unnamed `labels` (e.g. `list("Weight", "ECOG")`) are now used in the
  order of `vars`; before they were silently ignored.

- `digits_cat` now only applies to percentages: counts are always shown
  without decimals (before, e.g. “3.0 (75.0%)” in
  [`summaryLevels()`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md)
  and
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md),
  and “3/4.0” for `stat_cat = "n_N"`).

- [`summaryLevels()`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md)
  now checks `group` (single existing column, at least two groups) and
  supports any number of groups (before, empty cells were blank with 4
  or more groups). Rows with a missing group are removed with a message
  instead of being shown as an extra group “0”.

- Character variables are now shown in natural order in
  [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md)
  (e.g. “3” before “10”, “\< 10%” before “20%”). Factors keep their
  level order.

- Visits in
  [`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
  are now sorted naturally (e.g. “Visit 2” before “Visit 10”) when
  `order` is not given, whatever the name of the visit column.

- [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md)
  only computes p-values when `test = TRUE`; Fisher’s exact test no
  longer fails for variables with many categories.

- Bug fixes

## summarySCI 0.1.2

CRAN release: 2026-09-14

Removed the deprecated options from `gtsummary`

## summarySCI 0.1.0

Name changed from `summarySAKK` to `summarySCI`.
