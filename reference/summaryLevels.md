# summaryLevels

Collapses factor levels from multiple columns into one and creates
summary table.

## Usage

``` r
summaryLevels(
  data,
  vars = NULL,
  group = NULL,
  label = NULL,
  levels = NULL,
  stat_cat = "n_percent",
  test = FALSE,
  test_cat = "fisher.test",
  ci = FALSE,
  ci_cat = "wilson",
  conf_level = 0.95,
  digits_cat = 0,
  overall = FALSE,
  as_flex_table = TRUE,
  border = TRUE,
  word_output = FALSE,
  file_name = paste0("SummaryLevels_", format(Sys.Date(), "%Y%m%d"), ".docx")
)
```

## Arguments

- data:

  A data frame or tibble containing the data to be summarized.

- vars:

  Variables to include in the summary table. Need to be specified with
  quotes, e.g. `"score"` or `c("score", "age_cat")`. Default to all
  variables present in the data except `group`.

- group:

  A single column from `data`. Need to be specified with quotes, e.g.
  `"treatment"`. Summary statistics will be stratified according to this
  variable. Default to NULL.

- label:

  A label for the new variable to be created. If no label present, the
  variable name is taken.

- levels:

  = A vector containing the values indicating presence of the factor
  level. Included by default are "1", "yes", "Yes".

- stat_cat:

  Summary statistic to display for categorical variables. Options
  include "n_percent" (default) and "n", and "n_N".

- test:

  Logical. Indicates whether p-values are displayed (TRUE) or not
  (FALSE). Default to FALSE

- test_cat:

  Test type used to calculated the p-value for categorical variables.
  Only used if `test = TRUE`. Options include "fisher.test" (default),
  "chisq.test", "chisq.test.no.correct". If NULL, the function decides
  itself: "chisq.test.no.correct" for categorical variables with all
  expected cell counts \>=5, and "fisher.test" for categorical variables
  with any expected cell count \<5.

- ci:

  Logical. Indicates whether CI are displayed (TRUE) or not (FALSE).
  Default to FALSE.

- ci_cat:

  Confidence interval method for categorical variables. Options include
  "wilson" (default), "wilson.no.correct", "clopper.pearson", "wald",
  "wald.no.correct", "agresti.coull" and "jeffreys". If NULL, no CI will
  be displayed.

- conf_level:

  Numeric. Confidence level. Default to 0.95.

- digits_cat:

  Numeric. Digits for summary statistics and CI of categorical
  variables. Default to 0.

- overall:

  Logical. If TRUE, an additional column with the total is added to the
  table. Default to FALSE.

- as_flex_table:

  Logical. If TRUE (default) the gtsummary object is converted to a
  flextable object. Useful when rendering to Word.

- border:

  Logical. If TRUE, a border will be drawn around the table. Only
  available if flex_table = TRUE. Default is TRUE.

- word_output:

  Logical. If TRUE, the table is also saved in a word document.

- file_name:

  Character string. Specify the name of the Word document containing the
  table. Only used when `word_output` is TRUE. Needs to end with
  ".docx".

## Value

A table of class "`flextable`" or `c("tbl_stack", "gtsummary")`.
Optionally returns a .docx file in the specified folder.
