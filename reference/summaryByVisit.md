# Creates publication-ready summary tables for continuous data grouped, by visit

Creates publication-ready summary tables for continuous data grouped, by
visit

## Usage

``` r
summaryByVisit(
  data,
  vars = NULL,
  group = NULL,
  labels = NULL,
  stat_cont = "median_range",
  visit = "visit",
  order = NULL,
  visitgroup = NULL,
  digits_cont = 1,
  add_n = FALSE,
  overall = FALSE,
  as_flex_table = TRUE,
  border = TRUE,
  word_output = FALSE,
  file_name = paste0("SummaryByVisit_", format(Sys.Date(), "%Y%m%d"), ".docx")
)
```

## Arguments

- data:

  A data frame or tibble containing the data to be summarized.

- vars:

  Continuous variables to include in the summary table. Need to be
  specified with quotes, e.g. `"age"` or `c("age", "response")`. Default
  to all variables present in the data except `group`.

- group:

  A single column from `data`. Need to be specified with quotes, e.g.
  `"treatment"`. Summary statistics will be stratified according to this
  variable. Default to NULL. A maximum of 3 groups are currently
  supported.

- labels:

  A list containing the labels that should be used for the variables in
  the table. If NULL, labels are automatically taken from the dataset.
  If no label present, the variable name is taken.

- stat_cont:

  Summary statistic to display for continuous variables. Options include
  "median_IQR", "median_range" (default), "mean_sd", "mean_se" and
  "geomMean_sd".

- visit:

  Name of the stratum for which summary statistics are displayed by
  line. Typically, this would be `"visit"`.

- order:

  A numerical variable defining the visit order.

- visitgroup:

  A grouping variable for the stratum for which summary statistics are
  displayed by line. Must be an ordered factor. Typically, this would be
  a visit group such as e.g., baseline, follow-up etc.

- digits_cont:

  Digits for summary statistics and CI of continuous variables. Default
  to 1.

- add_n:

  Logical. If TRUE, an additional column with the total number of
  non-missing observations for each variable is added.

- overall:

  Logical. If TRUE, an additional column with the total is added to the
  table. Ignored, if no groups are defined. Default to FALSE.

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

A table of class "`flextable`" or
`c("tbl_strata_nested_stack", "tbl_stack", "gtsummary")`. Optionally
returns a .docx file in the specified folder.
