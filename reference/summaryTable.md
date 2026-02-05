# Creates publication-ready summary tables

Creates publication-ready summary tables based on the gtsummary package.

## Usage

``` r
summaryTable(
  data,
  vars = NULL,
  group = NULL,
  labels = NULL,
  stat_cont = "median_range",
  stat_cat = "n_percent",
  continuous_as = "continuous",
  dichotomous_as = "dichotomous",
  value = NULL,
  test = FALSE,
  test_cont = "wilcox.test",
  test_cat = "fisher.test",
  ci = FALSE,
  ci_cont = "wilcox.test",
  ci_cat = "wilson",
  conf_level = 0.95,
  digits_cont = 1,
  digits_cat = 0,
  missing = TRUE,
  missing_percent = TRUE,
  missing_text = "Missing",
  overall = FALSE,
  add_n = TRUE,
  as_flex_table = TRUE,
  border = TRUE,
  word_output = FALSE,
  file_name = paste0("SummaryTable_", format(Sys.Date(), "%Y%m%d"), ".docx")
)
```

## Arguments

- data:

  A data frame or tibble containing the data to be summarized.

- vars:

  Variables to include in the summary table. Need to be specified with
  quotes, e.g. `"age"` or `c("age", "response")`. Default to all
  variables present in the data except `group`.

- group:

  A single column from `data`. Need to be specified with quotes, e.g.
  `"treatment"`. Summary statistics will be stratified according to this
  variable. Default to NULL.

- labels:

  A list containing the labels that should be used for the variables in
  the table. If NULL, labels are automatically taken from the dataset.
  If no label present, the variable name is taken.

- stat_cont:

  Summary statistic to display for continuous variables. Options include
  "median_IQR", "median_range" (default), "mean_sd", "mean_se" and
  "geomMean_sd".

- stat_cat:

  Summary statistic to display for categorical variables. Options
  include "n_percent" (default) and "n", and "n_N".

- continuous_as:

  Type for the continuous variables. Can either be "continous" (default)
  or "categorical".

- dichotomous_as:

  Type for the dichotomous variables. Can either be "categorical"
  (default, one row per level) or "dichotomous" (only one row with
  reference level (see argument `value`), only works if
  `missing = "FALSE"` or `missing_percent = FALSE`.

- value:

  Specifies the reference level of a variable to display on a single
  row. Default is NULL. The syntax is as follows:
  `value = list(varname ~ "level to show")`.

- test:

  Logical. Indicates whether p-values are displayed (TRUE) or not
  (FALSE). Default to FALSE

- test_cont:

  Test type used to calculate the p-value for continuous variables. Only
  used if `test = TRUE`. Options include "t.test", "oneway.test",
  "kruskal.test", "wilcox.test" (default), "paired.t.test",
  "paired.wilcox.test"

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

- ci_cont:

  Confidence interval method for continuous variables. Only used if
  `ci = TRUE`. Options include "t.test" and "wilcox.test" (default).

- ci_cat:

  Confidence interval method for categorical variables. Options include
  "wilson" (default), "wilson.no.correct", "clopper.pearson", "wald",
  "wald.no.correct", "agresti.coull" and "jeffreys". If NULL, no CI will
  be displayed.

- conf_level:

  Numeric. Confidence level. Default to 0.95.

- digits_cont:

  Numeric. Digits for summary statistics and CI of continuous variables.
  Default to 1.

- digits_cat:

  Numeric. Digits for summary statistics and CI of categorical
  variables. Default to 0.

- missing:

  Logical. If TRUE (default), the missing values are shown.

- missing_percent:

  Indicates whether percentages for missings are shown (TRUE, default)
  or not (FALSE) for categorical variables. If "both", then both options
  are displayed next to each other.

- missing_text:

  String indicating text shown on missing row. Default to "Missing".

- overall:

  Logical. If TRUE, an additional column with the total is added to the
  table. Default to FALSE.

- add_n:

  Logical. If TRUE (default), an additional column with the total number
  of non-missing observations for each variable is added.

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

A table of class "`flextable`" or `c("tbl_summary", "gtsummary")`.
Optionally returns a .docx file in the specified folder.

## Examples

``` r
library(survival)
data("cancer")
summaryTable(data = cancer,vars = c("inst", "time","age", "ph.ecog"),
             labels = list(inst = "Institution code",
                           time = "Time",
                           age = "Age",
                           ph.ecog = "ECOG score"))


.cl-1ae65cc4{}.cl-1addd7ac{font-family:'DejaVu Sans';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-1addd7c0{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-1addd7ca{font-family:'DejaVu Sans';font-size:6.6pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;position: relative;bottom:3.3pt;}.cl-1ae14126{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-1ae14130{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-1ae1413a{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-1ae1413b{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-1ae14144{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:15pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-1ae1414e{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-1ae174e8{width:2.303in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae174f2{width:0.892in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae174f3{width:2.805in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae174fc{width:2.303in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae174fd{width:0.892in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae174fe{width:2.805in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae17506{width:2.303in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae1752e{width:0.892in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae1752f{width:2.805in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae17530{width:2.303in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae17531{width:0.892in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae17538{width:2.805in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae17539{width:2.303in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae1753a{width:0.892in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae17542{width:2.805in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae17543{width:2.303in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae17544{width:0.892in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1ae1754c{width:2.805in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Characteristic
```

N

N = 2281

Institution code

227

11.0 (1.0, 33.0)

Missing

1

Time

228

255.5 (5.0, 1,022.0)

Age

228

63.0 (39.0, 82.0)

ECOG score

227

1.0 (0.0, 3.0)

Missing

1

1Median (Min, Max)
