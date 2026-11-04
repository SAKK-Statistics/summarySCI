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
  ci_cat = "clopper.pearson",
  conf_level = 0.95,
  digits_cat = 0,
  overall = FALSE,
  add_n = TRUE,
  as_flex_table = TRUE,
  border = TRUE,
  word_output = FALSE,
  file_path = paste0("SummaryLevels_", format(Sys.Date(), "%Y%m%d"), ".docx")
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
  variable. Any number of groups (at least two) is possible. Rows with a
  missing value in `group` are removed. Default to NULL.

- label:

  A label for the new variable to be created. If no label present, the
  variable name is taken.

- levels:

  = A vector containing the values indicating presence of the factor
  level. Included by default are "1", "yes", "Yes". All variables in
  `vars` must be binary (e.g. 0/1, yes/no, TRUE/FALSE), unless `levels`
  is given; otherwise an error is returned.

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
  "clopper.pearson" (default), "wilson", "wilson.no.correct", "wald",
  "wald.no.correct", "agresti.coull" and "jeffreys". If NULL, no CI will
  be displayed.

- conf_level:

  Numeric. Confidence level. Default to 0.95.

- digits_cat:

  Numeric. Digits for summary statistics and CI of categorical
  variables. Default to 0.

- overall:

  Logical. If TRUE, an additional column with the total is added at the
  end of the table. Default to FALSE.

- add_n:

  Logical. If TRUE (default), a column with the number of patients is
  added before each group column (and before the Overall column). As
  missing values are treated as absent, N is the number of patients in
  each group.

- as_flex_table:

  Logical. If TRUE (default) the gtsummary object is converted to a
  flextable object. Useful when rendering to Word.

- border:

  Logical. If TRUE, a border will be drawn around the table. Only
  available if flex_table = TRUE. Default is TRUE.

- word_output:

  Logical. If TRUE, the table is also saved in a word document.

- file_path:

  Character string. Specify the path of the Word document containing the
  table. Only used when `word_output` is TRUE. Needs to end with
  ".docx".

## Value

A table of class "`flextable`" or `c("tbl_stack", "gtsummary")`.
Optionally returns a .docx file in the specified folder.

## Examples

``` r
# Site of progression: one column per site, more than one site possible
progression <- data.frame(
  arm   = rep(c("A", "B"), each = 5),
  liver = c("absent", "present", "absent", "present", "absent",
            "absent", "present", "absent", "present", "absent"),
  lung  = c("absent", "absent", "present", "absent", "absent",
            "absent", "absent", "absent", "absent", "absent"),
  brain = c("present", "absent", "present", "present", "present",
            "present", "present", "present", "present", "present")
)

summaryLevels(data = progression,
              vars = c("liver", "lung", "brain"),
              label = "Site of progression",
              levels = "present")


.cl-d8bad469{}.cl-8a796ab5{font-family:'DejaVu Sans';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-56775354{font-family:'DejaVu Sans';font-size:6.6pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:super;}.cl-5430bca3{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f4afa9ce{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-df282d82{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c6655fef{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-03fa5271{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-6477613b{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a2472f46{width:3.525in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d3d41b10{width:0.855in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bffbc2f1{width:1.62in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-63ae8e82{width:3.525in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7a5c8b47{width:0.855in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-16fd77f0{width:1.62in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-187a98ef{width:3.525in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-32b17d58{width:0.855in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-06f07ebc{width:1.62in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-2241085e{width:3.525in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8d3fb2d0{width:0.855in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-818a0cd4{width:1.62in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Site of progression1
```
