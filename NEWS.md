# summarySCI 1.0.0

- `stat_cont = "geomMean_sd"` now shows the geometric mean with the geometric
  standard deviation, `exp(sd(log(x)))`, instead of the arithmetic standard
  deviation, and both are `NA` if a value is zero or negative (instead of the
  geometric mean of the positive values only). The geometric SD, a factor
  close to 1, is shown with at least 2 decimals. The footnotes read "Geometric
  mean (geometric SD)" and "Mean (SE)".

- Improvement of the internal structure of the function 

- More informative error messages 

- No more upper limit for the number of groups in `summaryTable()`.

- `summaryByVisit()` now also accepts categorical variables.

- Argument `file_name` was changed to `file_path`

- New function `boxplotSCI()`

- Intensive tests have been added

- The default of `digits_cat` in `summaryByVisit()` was changed from 1 to 0,
  consistent with `summaryTable()` and `summaryLevels()`.

- The default of `add_n` in `summaryByVisit()` was changed from FALSE to TRUE,
  consistent with `summaryTable()`.

- In `summaryLevels()`, the Overall column is now shown at the end of the
  table (with the p-value before it), consistent with the other functions.

- `summaryLevels()` now gives an informative error if a variable is not binary
  (e.g. 0/1, yes/no) and `levels` is not given, instead of showing counts of 0.
  Several values in `levels` are now all counted as present, and yes/no
  variables with missing values are handled correctly.

- New argument `add_n` in `summaryLevels()` (default TRUE), consistent with
  `summaryTable()` and `summaryByVisit()`.

- The default CI method of `summaryLevels()` was changed from Wilson to
  Clopper-Pearson (`ci_cat = "clopper.pearson"`), consistent with
  `summaryTable()`. `ci_cat = "clopper.pearson"` failed before in
  `summaryLevels()` and now works.

- In `summaryByVisit()`, unnamed `labels` (e.g. `list("Weight", "ECOG")`)
  are now used in the order of `vars`; before they were silently ignored.

- `digits_cat` now only applies to percentages: counts are always shown
  without decimals (before, e.g. "3.0 (75.0%)" in `summaryLevels()` and
  `summaryByVisit()`, and "3/4.0" for `stat_cat = "n_N"`).

- `summaryLevels()` now checks `group` (single existing column, at least two
  groups) and supports any number of groups (before, empty cells were blank
  with 4 or more groups). Rows with a missing group are removed with a
  message instead of being shown as an extra group "0".

- CI limits of categorical variables now have exactly `digits_cat` decimals
  in `summaryTable()` and `summaryLevels()` (before, one more below 10%).

- Character variables are now shown in natural order in `summaryTable()`,
  `summaryByVisit()` and `stackedbarplotSCI()` (e.g. "3" before "10",
  "< 10%" before "20%", "Arm 2" before "Arm 10"). Factors keep their level order.

- Visits in `summaryByVisit()` are now sorted naturally (e.g. "Visit 2" before
  "Visit 10") when `order` is not given, whatever the name of the visit column.

- `summaryTable()` only computes p-values when `test = TRUE`; Fisher's exact
  test no longer fails for variables with many categories.

- Bug fixes


# summarySCI 0.1.2

Removed the deprecated options from `gtsummary`

# summarySCI 0.1.0

Name changed from `summarySAKK` to `summarySCI`.
