# Line plot of a summary statistic over time, by group

Generates line plots of a central-tendency statistic over time for one
or multiple sub-groups, optionally with a dispersion band (ribbon or
whiskers) and a table of patient numbers beneath the horizontal axis.

## Usage

``` r
lineplotSCI(
  data,
  var,
  order,
  group = NULL,
  visit = NULL,
  n_order = NULL,
  group_names = NULL,
  group_names_short = NULL,
  stat = c("median", "mean", "geomean"),
  interval = c("iqr", "range", "ci", "sd", "se", "none"),
  interval_type = c("whiskers", "ribbon"),
  alpha = 0.05,
  dodge = 0,
  connect = TRUE,
  connect_shade = 0.15,
  main = "",
  xlab = "Time",
  ylab = "Variable",
  xlim = NULL,
  ylim = NULL,
  y_ticks = NULL,
  col = NULL,
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
  lines = NULL,
  lty = 1,
  col_lines = "grey",
  legend_position = "topright",
  legend_title = NULL,
  legend_offset = c(0, 0),
  legend_cex = 1,
  legend_bty = "n",
  ...
)
```

## Arguments

- data:

  A data frame or tibble containing the data to be plotted.

- var:

  Name of the column in `data` indicating the measurement to be plotted.
  Supports bare or quoted column names.

- order:

  Name of the column in `data` containing the time points of interest.
  The time points must be numeric or coercible to numeric, since they
  are used as the plotting coordinates on the x-axis. Rows with a
  missing time point are dropped.

- group:

  Name of the column in `data` containing the grouping variable (e.g.,
  treatment arms). Supports bare or quoted column names. Rows with a
  missing group are dropped. Default is `NULL`, which draws a single
  line, labelled with `var` (or `group_names`).

- visit:

  Descriptive labels for the time points. Either the name of a column in
  `data` containing the labels (bare or quoted; the first label observed
  at each time point is used), or a character vector with one entry per
  tick position. Default is `NULL`, which prints the time values
  themselves.

- n_order:

  A numeric value indicating the number of tick positions on the x-axis.
  Default is `NULL`, which places one tick at each distinct value of
  `order`. Set explicitly to reserve ticks for time points with no
  observed data, in which case ticks are drawn at `1:n_order`.

- group_names:

  A character vector containing the names of the groups to be displayed
  in the legend. The order must match `levels(factor(data$group))`.
  Default is `NULL`, which falls back to the group names found in the
  data.

- group_names_short:

  A character vector containing short names or abbreviations for the
  groups, to be displayed below the x-axis near the patient counts.
  Optional. Defaults to `group_names`.

- stat:

  The summary statistic plotted as the point estimate: `"median"` (the
  default), `"mean"` or `"geomean"` (geometric mean, for positive
  values).

- interval:

  The quantity spanned by the error band around the point estimate. One
  of `"iqr"` (the default, 25th to 75th percentile, computed as in the
  summary tables, i.e. `quantile(type = 2)`), `"range"` (observed
  minimum to maximum), `"ci"` (a two-sided confidence interval at level
  `1 - alpha`), `"sd"` (point estimate +/- one standard deviation),
  `"se"` (point estimate +/- one standard error), or `"none"` to plot
  point estimates alone. With `stat = "geomean"`, only `"iqr"`,
  `"range"` and `"none"` are available.

- interval_type:

  The style in which the error band is drawn. Either `"whiskers"` (the
  default; vertical bars with caps) or `"ribbon"` (a shaded polygon with
  a dashed border). Ignored when `interval` is `"none"`.

- alpha:

  The two-sided significance level used when `interval = "ci"`. Default
  is `0.05`, giving 95% confidence intervals. For `stat = "mean"` the
  interval is the usual t-interval; for `stat = "median"` it is the
  distribution-free interval based on the binomial order statistics,
  which is undefined for very small cells and returned as `NA` there.

- dodge:

  A numeric value giving the total horizontal offset applied across
  groups, in x-axis units, so that overlapping error bars remain
  readable. Default is `0` (all groups drawn at the same x position).

- connect:

  A logical toggle switch controlling whether the point estimates within
  a group are joined by a line. Default is `TRUE`.

- connect_shade:

  A numeric value between `0` and `1` giving how much lighter the
  connecting lines are drawn than the point estimates. The color is
  mixed with white rather than made transparent, so the lines stay
  opaque where they cross error bars or a ribbon. Default is `0.15` (15%
  lighter); `0` draws the lines in the group color.

- main:

  Title of the plot. Defaults to `""` (no title).

- xlab:

  A label for the x-axis. Default is `"Time"`.

- ylab:

  A label for the y-axis. Default is `"Variable"`.

- xlim:

  The x-axis limits of the plot as a numeric vector of length 2. Default
  is `NULL`, which spans all tick positions and observed time points.

- ylim:

  The y-axis limits of the plot as a numeric vector of length 2. Default
  is `NULL`, which adds a 10% padding buffer above and below the data
  range (band included, where one is drawn).

- y_ticks:

  A numeric vector specifying the explicit tick positions on the y-axis.
  Default is `NULL`, which falls back to automatic tick placement.

- col:

  A character vector of hex colors or color names to use for the groups.
  Recycled if shorter than the number of groups. Default is `NULL`,
  which automatically applies the SCI color palette.

- cex_lab:

  A numeric value specifying the font size of the main axis labels.
  Default is `1`.

- cex_axis:

  A numeric value specifying the font size of the axis tick marks.
  Default is `1`.

- cex_n_patients:

  A numeric value specifying the font size of the sample size labels and
  numbers displayed beneath the horizontal axis. Default is `0.7`.

- risk_table:

  A logical toggle switch enabling the table of patient numbers
  (non-missing observations of `var` per group and time point) beneath
  the x-axis. Default is `FALSE`.

- missing_nr:

  A numeric or character value indicating how zero-count patient sizes
  on the x-axis should be displayed. Default is `0`. Can also be set to
  `NA`, which leaves the cell blank.

- rotate_labels:

  A logical toggle switch rotating the x-axis tick labels by 90 degrees.
  Default is `FALSE`. Bottom margin and label offsets are adjusted
  accordingly.

- mar_custom:

  A numeric vector indicating the plot margins to be used in the form
  `c(bottom, left, top, right)`. Default is `NULL`, which dynamically
  calculates spacing so that stacked sample-size rows are never clipped.

- xlab_line:

  A numeric value indicating the margin line offset for the x-axis
  label. Default is `NULL`, which calculates an adaptive offset based on
  the number of groups and on `rotate_labels`.

- ylab_line:

  A numeric value indicating the margin line offset for the y-axis
  label. Default is `3.0`.

- xlab_position:

  Horizontal position coordinate adjustment for the sample size group
  headers down the left-hand margin. Default is `NULL`, which
  auto-aligns them just outside the plot region.

- lines:

  A numeric vector indicating the vertical coordinates where horizontal
  reference lines will be plotted. Optional.

- lty:

  A numeric vector of length 1 or of the same length as `lines`
  indicating the line type to be plotted. Default is `1` (solid).

- col_lines:

  A character vector of length 1 or of the same length as `lines`
  indicating the color of the reference lines. Default is `"grey"`.

- legend_position:

  Position of the legend box. Options include standard base R keywords
  (e.g., `"topright"`, `"topleft"`, `"bottom"`) or manual numeric
  coordinate pairs `c(x, y)`. Default is `"topright"`. NULL removes the
  legend.

- legend_title:

  Title of the legend box. Default is `NULL` (no title).

- legend_offset:

  A numeric vector of length 2 specifying an `inset` to nudge the legend
  position away from plot elements. Default is `c(0, 0)`. Ignored when
  `legend_position` is given as coordinates.

- legend_cex:

  A numeric value specifying the font and element scaling size inside
  the legend box. Default is `1`.

- legend_bty:

  The type of box border to be drawn around the legend. Allowed values
  are `"n"` (transparent/no border, the default) and `"o"` (standard box
  frame).

- ...:

  Further graphical parameters passed on to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly, a list with the group-by-time matrices `centre`, `lower`,
`upper` (both all-`NA` when `interval` is `"none"`) and `n`, the number
of non-missing observations per cell.
