# Summarize data as a table and plots

Produces the summary table of
[`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md)
and one plot per variable, or either of them alone (see `type`): a
boxplot for continuous variables
([`boxplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/boxplotSCI.md))
and a stacked barplot for categorical and dichotomous ones
([`stackedbarplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/stackedbarplotSCI.md)).
The type of each plot follows the type the variable has in the table, so
that table and plots always agree.

## Usage

``` r
summarizeSCI(
  data,
  vars = NULL,
  group = NULL,
  labels = NULL,
  stat_cont = "median_range",
  stat_cat = "n_percent",
  missing = TRUE,
  missing_percent = TRUE,
  missing_text = "Missing",
  type = c("both", "table", "plot"),
  plot_args = list(),
  as_flex_table = TRUE,
  border = TRUE,
  word_output = FALSE,
  file_path = paste0("SummaryTable_", format(Sys.Date(), "%Y%m%d"), ".docx"),
  ...
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
  "geomMean_sd". "geomMean_sd" shows the geometric mean with the
  geometric standard deviation, `exp(sd(log(x)))`, both `NA` if a value
  is zero or negative. The geometric SD is a factor close to 1 and is
  shown with at least 2 decimals.

- stat_cat:

  Summary statistic to display for categorical variables. Options
  include "n_percent" (default) and "n", and "n_N".

- missing:

  Logical. If TRUE (default), the missing values are shown.

- missing_percent:

  Indicates whether percentages for missings are shown (TRUE, default)
  or not (FALSE) for categorical variables. If "both", then both options
  are displayed next to each other.

- missing_text:

  String indicating text shown on missing row. Default to "Missing".

- type:

  What to produce: `"both"` (the default) the table and a plot per
  variable, `"table"` the table alone, or `"plot"` the plots alone. The
  table is built in every case, since it decides the type of each plot.

- plot_args:

  A named list of further arguments for the plotting functions, e.g.
  `list(risk_table = TRUE, col = c("grey", "black"))`. Each plot
  receives the arguments its function has, as in
  [`boxplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/boxplotSCI.md)
  and
  [`stackedbarplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/stackedbarplotSCI.md);
  an argument no plot uses gives a warning. For example,
  `stat_footnote = TRUE` adds a footnote saying what each plot shows
  (the box and whiskers, or the denominator of the percentages).

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

- ...:

  Further arguments passed on to
  [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md),
  e.g. `test`, `ci`, `overall` or `digits_cont`.

## Value

An object of class `summarizeSCI`, a list with

- `table`: the table, a flextable or (with `as_flex_table = FALSE`) a
  gtsummary object; `NULL` with `type = "plot"`. With
  `word_output = TRUE` it is also saved to `file_path`, as
  [`summaryTable()`](https://sakk-statistics.github.io/summarySCI/reference/summaryTable.md)
  does;

- `plots`: a named list with one plot per variable, e.g. `res$plots$hb`;
  empty with `type = "table"`. Printing a plot draws it;
  [`save_plots()`](https://sakk-statistics.github.io/summarySCI/reference/save_plots.md)
  writes them to files.

Printing the result shows the table and lists the plots.

## Details

The plots are drawn from the same rows as the table, and the box of the
boxplot spans the quartiles as the table computes them (see
[`boxplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/boxplotSCI.md)).
The settings of the table carry over to the plots: the variable labels
become the axis or legend titles, `missing` / `missing_percent` decide
whether missing values form a segment of their own (as they are counted
in the table's percentages), and `stat_cat = "n"` shows counts instead
of percentages. Options that only concern the plots go into `plot_args`.

## Examples

``` r
library(summarySCI)
data("hemoglobin_data")
d <- hemoglobin_data[hemoglobin_data$visit_nr == 2, ]
d$hb_cat <- cut(d$hb, c(-Inf, 120, 140, Inf),
                labels = c("< 120", "120-140", "> 140"))
res <- summarizeSCI(d, vars = c("hb", "hb_cat"), group = "Arm")
res$table


.cl-d8bad469{}.cl-8a796ab5{font-family:'DejaVu Sans';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-56775354{font-family:'DejaVu Sans';font-size:6.6pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:super;}.cl-5430bca3{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f4afa9ce{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-df282d82{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c6655fef{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-03fa5271{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-d04581b7{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:15pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-6477613b{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-268cb553{width:1.471in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3b3aa1ed{width:0.474in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fca2a1c{width:1.839in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1c51f2a6{width:1.743in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-afc05cec{width:1.471in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-da4bff05{width:0.474in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e12cd1e9{width:1.839in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d98bb2bf{width:1.743in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-756b7f5d{width:1.471in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-51128a61{width:0.474in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5c06c29a{width:1.839in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-45efdab0{width:1.743in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8d44e2ec{width:1.471in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6d4592bf{width:0.474in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-b08e4517{width:1.839in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-347c7147{width:1.743in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Characteristic
```
