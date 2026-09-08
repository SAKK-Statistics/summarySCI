#' Line plot of a summary statistic over time, by group
#'
#' Generates line plots of a summary statistic over time for one or
#' multiple sub-groups, optionally with a dispersion band (ribbon or whiskers)
#' and a table of patient numbers beneath the horizontal axis.
#'
#' @param data A data frame or tibble containing the data to be plotted.
#' @param var Name of the column in `data` indicating the measurement to be
#' plotted. Supports bare or quoted column names.
#' @param order Name of the column in `data` containing the time points of interest.
#' The time points must be numeric or coercible to numeric, since they are used
#' as the plotting coordinates on the x-axis. Rows with a missing time point
#' are dropped.
#' @param order_labels Descriptive labels for the time points. Either a character
#' vector with one entry per tick position, or the name of a column in `data`
#' containing the labels (in which case the first label observed at each time
#' point is used). Default is `NULL`, which prints the time values themselves.
#' @param n_order A numeric value indicating the number of tick positions on the
#' x-axis. Default is `NULL`, which places one tick at each distinct value of
#' `order`. Set explicitly to reserve ticks for time points with no observed data,
#' in which case ticks are drawn at `1:n_order`.
#' @param group Name of the column in `data` containing the grouping variable
#' (e.g., treatment arms). Supports bare or quoted column names. Rows with a
#' missing group are dropped.
#' @param group_names A character vector containing the names of the groups
#' to be displayed in the legend. The order must match `levels(factor(data$group))`.
#' Default is `NULL`, which falls back to the group names found in the data.
#' @param group_names_short A character vector containing short names or abbreviations
#' for the groups, to be displayed below the x-axis near the patient counts.
#' Optional. Defaults to `group_names`.
#' @param stat The summary statistic plotted as the point estimate. Either
#' `"mean"` (the default) or `"median"`. Determines the meaning of the dispersion
#' band: mean +/- standard deviation, or the observed minimum-maximum range.
#' @param ci_type The style in which the dispersion band is drawn. Either
#' `"ribbon"` (a shaded polygon with a dashed border) or `"whiskers"` (vertical
#' error bars with caps). Default is `NULL`, which suppresses the band and plots
#' point estimates only.
#' @param connect A logical toggle switch controlling whether the point estimates
#' within a group are joined by a line. Default is `TRUE`.
#' @param main Title of the plot. Defaults to `""` (no title).
#' @param xlab A label for the x-axis. Default is `"Time"`.
#' @param ylab A label for the y-axis. Default is `"Variable"`.
#' @param ylim The y-axis limits of the plot as a numeric vector of length 2.
#' Default is `NULL`, which adds a 10% padding buffer above and below the data
#' range (band included, where one is drawn).
#' @param y_ticks A numeric vector specifying the explicit tick positions on the
#' y-axis. Default is `NULL`, which falls back to automatic tick placement.
#' @param col A character vector of hex colors or color names to use for the
#' groups. Recycled if shorter than the number of groups. Default is `NULL`,
#' which automatically applies the SCI color palette.
#' @param cex_lab A numeric value specifying the font size of the main axis labels. Default is `1`.
#' @param cex_axis A numeric value specifying the font size of the axis tick marks. Default is `1`.
#' @param cex_n_patients A numeric value specifying the font size of the sample size
#' labels and numbers displayed beneath the horizontal axis. Default is `0.7`.
#' @param risk_table A logical toggle switch enabling the table of patient numbers
#' (non-missing observations of `var` per group and time point) beneath the
#' x-axis. Default is `FALSE`.
#' @param missing_nr A numeric or character value indicating how zero-count patient
#' sizes on the x-axis should be displayed. Default is `0`. Can also be set to
#' `NA`, which leaves the cell blank.
#' @param rotate_labels A logical toggle switch rotating the x-axis tick labels
#' by 90 degrees. Default is `FALSE`. Bottom margin and label offsets are adjusted
#' accordingly.
#' @param mar_custom A numeric vector indicating the plot margins to be used in
#' the form `c(bottom, left, top, right)`. Default is `NULL`, which dynamically
#' calculates spacing so that stacked sample-size rows are never clipped.
#' @param xlab_line A numeric value indicating the margin line offset for the x-axis label.
#' Default is `NULL`, which calculates an adaptive offset based on the number of
#' groups and on `rotate_labels`.
#' @param ylab_line A numeric value indicating the margin line offset for the y-axis label.
#' Default is `3.0`.
#' @param xlab_position Horizontal position coordinate adjustment for the sample size
#' group headers down the left-hand margin. Default is `NULL`, which auto-aligns them
#' just outside the plot region.
#' @param lines A numeric vector indicating the vertical coordinates where horizontal
#' reference lines will be plotted. Optional.
#' @param lty A numeric vector of length 1 or of the same length as `lines`
#' indicating the line type to be plotted. Default is `1` (solid).
#' @param col_lines A character vector of length 1 or of the same length as
#' `lines` indicating the color of the reference lines. Default is `"grey"`.
#' @param legend_position Position of the legend box. Options include standard base R
#' keywords (e.g., `"topright"`, `"topleft"`, `"bottom"`) or manual numeric coordinate pairs
#' `c(x, y)`. Default is `"topright"`.
#' @param legend_title Title of the legend box. Default is `NULL` (no title).
#' @param legend_offset A numeric vector of length 2 specifying an `inset` to nudge the
#' legend position away from plot elements. Default is `c(0, 0)`. Ignored when
#' `legend_position` is given as coordinates.
#' @param legend_cex A numeric value specifying the font and element scaling size
#' inside the legend box. Default is `1`.
#' @param legend_bty The type of box border to be drawn around the legend.
#' Allowed values are `"n"` (transparent/no border, the default) and `"o"` (standard box frame).
#' @param ... Further graphical parameters passed on to `plot()`.
#'
#' @return Invisibly, a list with the group-by-time matrices `centre`, `lower`,
#' `upper` (both `NULL` when `ci_type` is `NULL`) and `n` (`NULL` when
#' `risk_table` is `FALSE`).
#'
#' @export
lineplotSCI <- function(data, var, order, group,
                      order_labels = NULL, n_order = NULL,
                      group_names = NULL, group_names_short = NULL,
                      stat = c("mean", "median"), ci_type = NULL, connect = TRUE,
                      main = "", xlab = "Time", ylab = "Variable",
                      ylim = NULL, y_ticks = NULL, col = NULL,
                      cex_lab = 1, cex_axis = 1, cex_n_patients = 0.7,
                      risk_table = FALSE, missing_nr = 0, rotate_labels = FALSE,
                      mar_custom = NULL, xlab_line = NULL, ylab_line = 3.0,
                      xlab_position = NULL,
                      lines = NULL, lty = 1, col_lines = "grey",
                      legend_position = "topright", legend_title = NULL,
                      legend_offset = c(0, 0), legend_cex = 1, legend_bty = "n",
                      ...) {

  # Accept both quoted ("x") and unquoted (x) column names
  as_name <- function(expr) if (is.character(expr)) expr else deparse(expr)
  var_name   <- as_name(substitute(var))
  order_name <- as_name(substitute(order))
  group_name <- as_name(substitute(group))

  stat <- match.arg(stat)
  if (!is.null(ci_type)) ci_type <- match.arg(ci_type, c("ribbon", "whiskers"))
  if (is.null(col)) col <- c("#44087C", "#BA9CED")   # SCI palette

  v <- data[[var_name]]
  g <- data[[group_name]]
  o <- data[[order_name]]

  keep <- !is.na(g) & !is.na(o)
  v <- v[keep]; g <- factor(g[keep]); o <- as.numeric(o[keep])

  # --- Summarise: point estimate and dispersion per group and time point -----
  if (stat == "median") {
    centre <- tapply(v, list(g, o), median, na.rm = TRUE)
    lower  <- suppressWarnings(tapply(v, list(g, o), min, na.rm = TRUE))
    upper  <- suppressWarnings(tapply(v, list(g, o), max, na.rm = TRUE))
  } else {
    centre <- tapply(v, list(g, o), mean, na.rm = TRUE)
    s      <- tapply(v, list(g, o), sd,   na.rm = TRUE)
    lower  <- centre - s
    upper  <- centre + s
  }
  lower[!is.finite(lower)] <- NA
  upper[!is.finite(upper)] <- NA

  groups   <- rownames(centre)                 # sorted factor levels
  n_groups <- length(groups)
  if (!is.null(group_names)) {
    if (length(group_names) != n_groups)
      stop("'group_names' must have one entry per group (", n_groups, ").")
    groups <- group_names
  }
  groups_short <- if (is.null(group_names_short)) groups else group_names_short
  cols <- rep(col, length.out = n_groups)

  xs   <- as.numeric(colnames(centre))         # observed time points
  at_x <- if (is.null(n_order)) xs else seq_len(n_order)

  # --- Axis labels: character vector or a column of 'data' -------------------
  if (is.null(order_labels)) {
    order_labels <- as.character(at_x)
  } else if (length(order_labels) == 1L && is.character(order_labels) &&
             order_labels %in% names(data)) {
    lab <- as.character(data[[order_labels]][keep])
    order_labels <- as.character(tapply(lab, o, function(z) z[1L]))
  }
  if (length(order_labels) != length(at_x))
    stop("'order_labels' must have one entry per tick position (",
         length(at_x), ").")

  # --- Axis ranges -----------------------------------------------------------
  if (!is.null(ci_type)) {
    rng <- range(c(lower, upper), na.rm = TRUE)
  } else {
    rng <- range(centre, na.rm = TRUE)
  }
  if (is.null(ylim)) {
    pad  <- if (diff(rng) > 0) 0.1 * diff(rng) else 0.1 * abs(rng[1]) + 0.1
    ylim <- c(rng[1] - pad, rng[2] + pad)
  }

  # --- Plot region: extra bottom margin for rotated labels and/or the
  #     sample-size rows, extra left margin for their row labels --------------
  if (is.null(mar_custom)) {
    bottom <- if (rotate_labels) 7 else 5
    left   <- max(4.5, ylab_line + 1.5)
    if (risk_table) {
      bottom    <- bottom + n_groups + 2.5
      lab_chars <- max(nchar(c("No. of patients", groups_short)))
      left      <- max(left, 1.5 + 0.45 * lab_chars * cex_n_patients)
    }
    mar_custom <- c(bottom, left, if (nzchar(main)) 3 else 2, 2)
  }
  op <- par(mar = mar_custom)
  on.exit(par(op), add = TRUE)

  plot(NULL,
       xlim = range(c(at_x, xs)), ylim = ylim, main = main,
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
  usr <- par("usr")
  box()

  if (is.null(y_ticks)) {
    axis(2, las = 1, cex.axis = cex_axis)
  } else {
    axis(2, at = y_ticks, las = 1, cex.axis = cex_axis)
  }
  axis(1, at = at_x, labels = order_labels, cex.axis = cex_axis,
       las = if (rotate_labels) 2 else 1)
  mtext(ylab, side = 2, line = ylab_line, cex = cex_lab)

  # --- Horizontal reference lines (drawn below the data) ---------------------
  if (!is.null(lines)) {
    abline(h = lines,
           lty = rep(lty,       length.out = length(lines)),
           col = rep(col_lines, length.out = length(lines)))
  }

  # --- Ribbon (drawn first so points/lines sit on top) ----------------------
  if (!is.null(ci_type) && ci_type == "ribbon") {
    for (i in seq_len(n_groups)) {
      ok <- !is.na(centre[i, ]) & !is.na(lower[i, ]) & !is.na(upper[i, ])
      if (any(ok)) {
        polygon(c(xs[ok], rev(xs[ok])),
                c(lower[i, ok], rev(upper[i, ok])),
                col = adjustcolor(cols[i], alpha.f = 0.05),
                border = cols[i], lty = 2)
      }
    }
  }

  # --- Lines, points, whiskers ----------------------------------------------
  for (i in seq_len(n_groups)) {
    if (isTRUE(connect))
      graphics::lines(xs, centre[i, ], col = cols[i], lwd = 2)
    points(xs, centre[i, ], col = cols[i], pch = 19, cex = 1.5)

    if (!is.null(ci_type) && ci_type == "whiskers") {
      ok <- !is.na(lower[i, ]) & !is.na(upper[i, ]) & (upper[i, ] > lower[i, ])
      if (any(ok)) {
        arrows(xs[ok], lower[i, ok], xs[ok], upper[i, ok],
               col = cols[i], angle = 90, code = 3, length = 0.05, lwd = 2)
      }
    }
  }

  # --- Sample sizes: non-missing observations per group and time point ------
  counts <- NULL
  if (risk_table) {
    counts <- tapply(!is.na(v), list(g, o), sum)
    counts[is.na(counts)] <- 0

    shown <- matrix(as.character(counts), nrow = n_groups)
    shown[counts == 0] <- if (is.na(missing_nr)) "" else as.character(missing_nr)

    header_line <- if (rotate_labels) 6 else 3
    lab_at <- if (is.null(xlab_position)) usr[1] - 0.02 * diff(usr[1:2])
    else xlab_position

    mtext("No. of patients", side = 1, line = header_line, at = lab_at,
          adj = 1, cex = cex_n_patients, font = 2, xpd = NA)

    for (i in seq_len(n_groups)) {
      ln <- header_line + i
      mtext(groups_short[i], side = 1, line = ln, at = lab_at, adj = 1,
            col = cols[i], cex = cex_n_patients, xpd = NA)
      mtext(shown[i, ], side = 1, line = ln, at = xs,
            col = "black", cex = cex_n_patients)
    }
  }

  # --- x-axis label, placed below rotated labels / sample-size rows ----------
  if (is.null(xlab_line)) {
    xlab_line <- if (risk_table) {
      (if (rotate_labels) 6 else 3) + n_groups + 1.5
    } else if (rotate_labels) 5.5 else 3
  }
  mtext(xlab, side = 1, line = xlab_line, cex = cex_lab)

  # --- Legend ----------------------------------------------------------------
  legend_args <- list(legend = groups, title = legend_title, title.adj = 0,
                      col = cols, pch = 19,
                      lwd = if (isTRUE(connect)) 2 else NA,
                      bty = legend_bty, cex = legend_cex,
                      bg = if (legend_bty == "n") NA else "white",
                      box.col = "grey70")
  if (is.numeric(legend_position) && length(legend_position) == 2L) {
    do.call(legend, c(list(x = legend_position[1], y = legend_position[2]),
                      legend_args))
  } else {
    do.call(legend, c(list(x = legend_position, inset = legend_offset),
                      legend_args))
  }

  invisible(list(centre = centre,
                 lower  = if (!is.null(ci_type)) lower else NULL,
                 upper  = if (!is.null(ci_type)) upper else NULL,
                 n      = counts))
}
