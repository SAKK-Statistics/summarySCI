# Draw a stacked barplot from pre-computed counts

Drawing back end shared by
[`stackedbarplotSCI()`](https://sakk-statistics.github.io/summarySCI/reference/stackedbarplotSCI.md),
which counts raw data, and
[`plotByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/plotByVisit.md),
which reads the counts off a
[`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
table.

## Usage

``` r
stackedbar_draw(
  counts,
  n,
  visit = dimnames(counts)[[3]],
  group_names = NULL,
  group_names_short = NULL,
  scale = "percent",
  main = "",
  xlab = NULL,
  ylab = NULL,
  ylim = NULL,
  y_ticks = NULL,
  col = NULL,
  greyscale = FALSE,
  percent_labels = TRUE,
  col_percent = NULL,
  cex_percent = NULL,
  digits = 0,
  bar_width = NULL,
  cex_lab = 1,
  cex_axis = 1,
  cex_n_patients = 0.7,
  risk_table = FALSE,
  missing_nr = 0,
  rotate_labels = FALSE,
  mar_custom = NULL,
  xlab_line = NULL,
  ylab_line = 3,
  xlab_position = NULL,
  legend_position = "top",
  legend_title = NULL,
  legend_offset = c(0, 0),
  legend_cex = 1,
  legend_bty = "n",
  ...
)
```

## Arguments

- counts:

  Numeric array of counts indexed by segment, group and time point, with
  dimnames.

- n:

  Numeric group-by-time matrix of denominators for the percentages.

- visit:

  Character vector of labels for the time points, one per column of the
  third dimension of `counts`.

- group_names:

  A character vector containing the names of the groups to be displayed
  on the x-axis. The order must match `levels(factor(data$group))`.
  Default is `NULL`, which falls back to the group names found in the
  data.

- group_names_short:

  A character vector containing short names or abbreviations for the
  groups, displayed beneath the bars and in the table of patient
  numbers. Optional. Defaults to `group_names`.

- scale:

  The quantity on the y-axis. Either `"percent"` (the default) or
  `"count"`.

- main:

  Title of the plot. Defaults to `""` (no title).

- xlab:

  A label for the x-axis. Default is `NULL`, which uses `"Time"` if
  there are several time points, otherwise `"Group"`.

- ylab:

  A label for the y-axis. Default is `NULL`, which uses
  `"Percentage of patients"` or `"Number of patients"` depending on
  `scale`.

- ylim:

  The y-axis limits of the plot as a numeric vector of length 2. Default
  is `NULL`, which spans `0` to `100` for percentages (more if the
  stacks exceed 100%) and `0` to the tallest stack for counts.

- y_ticks:

  A numeric vector specifying the explicit tick positions on the y-axis.
  Default is `NULL`, which falls back to automatic tick placement.

- col:

  A character vector of hex colors or color names to use for the stacked
  segments, bottom to top. Recycled if shorter than the number of
  segments. Default is `NULL`, which automatically applies the SCI color
  palette.

- greyscale:

  A logical toggle switch replacing the color palette by a sequential
  grey ramp, for print or black-and-white reproduction. Default is
  `FALSE`. Ignored if `col` is supplied.

- percent_labels:

  A logical toggle switch enabling the percentage labels printed inside
  the segments. Default is `TRUE`. Empty segments, and segments too thin
  to hold a label, are left unlabelled.

- col_percent:

  A character value giving the color of the percentage labels. Default
  is `NULL`, which picks black or white per segment, whichever contrasts
  better with the fill.

- cex_percent:

  A numeric value specifying the font size of the percentage labels.
  Default is `NULL`, which sizes them to fit the width of the bars (at
  most `0.8`). A number is used as given, even if labels then overflow
  narrow bars. The group labels beneath the bars always shrink to fit.

- digits:

  A numeric value indicating the number of decimal places shown in the
  percentage labels. Default is `0`.

- bar_width:

  A numeric value giving the width of each bar in x-axis units. Default
  is `NULL`, which fills about 70% of each time point with the cluster
  of group bars.

- cex_lab:

  A numeric value specifying the font size of the main axis labels.
  Default is `1`.

- cex_axis:

  A numeric value specifying the font size of the axis tick marks.
  Default is `1`.

- cex_n_patients:

  A numeric value specifying the font size of the group labels beneath
  the bars and of the table of patient numbers. Default is `0.7`.

- risk_table:

  A logical toggle switch enabling the table of patient numbers (the
  denominator of each bar) beneath the x-axis. Default is `FALSE`.

- missing_nr:

  A numeric or character value indicating how zero-count patient sizes
  on the x-axis should be displayed. Default is `0`. Can also be set to
  `NA`, which leaves the cell blank.

- rotate_labels:

  A logical toggle switch rotating the x-axis tick labels by 90 degrees.
  Default is `FALSE`.

- mar_custom:

  A numeric vector indicating the plot margins to be used in the form
  `c(bottom, left, top, right)`. Default is `NULL`, which dynamically
  calculates spacing for the labels, the table of patient numbers and
  the legend.

- xlab_line:

  A numeric value indicating the margin line offset for the x-axis
  label. Default is `NULL`, which calculates an adaptive offset.

- ylab_line:

  A numeric value indicating the margin line offset for the y-axis
  label. Default is `3.0`.

- xlab_position:

  Horizontal position of the row labels of the table of patient numbers.
  Default is `NULL`, which aligns them just outside the plot region.

- legend_position:

  Position of the legend box. `"top"` (the default) places it in one row
  above the bars, which keeps their full width; `"right"` places it
  outside the plot region to the right of the bars; `"none"` or `NULL`
  suppresses it. Other base R keywords (e.g., `"topright"`) place it
  inside the plot region, and a numeric pair `c(x, y)` gives explicit
  coordinates. The margins are sized to the legend text.

- legend_title:

  Title of the legend box. Default is `NULL` (no title).

- legend_offset:

  A numeric vector of length 2 specifying an `inset` to nudge a
  keyword-placed legend. Default is `c(0, 0)`.

- legend_cex:

  A numeric value specifying the font and element scaling size inside
  the legend box. Default is `1`.

- legend_bty:

  The type of box border to be drawn around the legend. Allowed values
  are `"n"` (no border, the default) and `"o"` (standard box frame).

- ...:

  Further graphical parameters passed on to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly, a list with `counts`, `n` and `percent`.
