#' Stacked barplot over time
#'
#' Generates stacked barplots of a categorical variable over time for one or
#' multiple sub-groups. Each bar shows the distribution of the levels of `var`
#' at one time point in one group, optionally with the percentages printed
#' inside the segments and a table of patient numbers beneath the horizontal
#' axis.
#'
#' Two layouts of the data are supported:
#' * `var` names **one** categorical column (long format): its levels are
#'   stacked, and percentages are relative to the number of non-missing
#'   observations in the bar (or all observations if `missing = TRUE`).
#' * `var` names **several** columns coded `"Yes"` / `"No"` (wide format, e.g.
#'   one column per adverse event): each column becomes one segment holding the
#'   number of `"Yes"` answers, and percentages are relative to the number of
#'   patients in the bar (`id`, or rows if `id` is `NULL`). Missing values
#'   count as `"No"`. Since a patient may answer `"Yes"` to several columns,
#'   the segments can add up to more than 100%.
#'
#' @param data A data frame or tibble containing the data to be plotted.
#' @param var Name of the column(s) in `data` to be stacked. A single column
#' supports bare or quoted names; several columns are given as a character
#' vector.
#' @param time Name of the column in `data` containing the time points of
#' interest. Numeric time points are sorted numerically; factors keep their
#' level order; character values are sorted by the number they contain (e.g.
#' `"Visit 2"` before `"Visit 10"`), or by order of appearance if they contain
#' none. If left empty, a single time point is assumed and one bar is drawn per
#' group. Rows with a missing time point are dropped.
#' @param time_labels Name of the column in `data` containing descriptive labels
#' for the time points (the first label observed at each time point is used).
#' Optional.
#' @param group Name of the column in `data` containing the grouping variable
#' (e.g., treatment arms). If left empty, a single group is assumed. Rows with
#' a missing group are dropped.
#' @param id Name of the column in `data` identifying the patient. Only used
#' when `var` names several Yes/No columns, as the denominator of the
#' percentages. Default is `NULL`, which counts rows instead.
#' @param group_names A character vector containing the names of the groups
#' to be displayed on the x-axis. The order must match `levels(factor(data$group))`.
#' Default is `NULL`, which falls back to the group names found in the data.
#' @param group_names_short A character vector containing short names or
#' abbreviations for the groups, displayed beneath the bars and in the table of
#' patient numbers. Optional. Defaults to `group_names`.
#' @param level_names A character vector containing the names of the stacked
#' segments to be displayed in the legend. The order must match the levels of
#' `var` (or the columns in `var`). Default is `NULL`, which uses the levels
#' (or column names) themselves.
#' @param scale The quantity on the y-axis. Either `"percent"` (the default) or
#' `"count"`.
#' @param missing A logical toggle switch adding missing values of `var` as a
#' segment of their own, on top of the stack. Default is `FALSE`, which drops
#' them. Only used when `var` is a single column.
#' @param missing_text Legend text for the missing-value segment. Default is
#' `"Missing"`.
#' @param main Title of the plot. Defaults to `""` (no title).
#' @param xlab A label for the x-axis. Default is `NULL`, which uses `"Time"`
#' if there are several time points, otherwise `"Group"`.
#' @param ylab A label for the y-axis. Default is `NULL`, which uses
#' `"Percentage of patients"` or `"Number of patients"` depending on `scale`.
#' @param ylim The y-axis limits of the plot as a numeric vector of length 2.
#' Default is `NULL`, which spans `0` to `100` for percentages (more if the
#' stacks exceed 100%) and `0` to the tallest stack for counts.
#' @param y_ticks A numeric vector specifying the explicit tick positions on the
#' y-axis. Default is `NULL`, which falls back to automatic tick placement.
#' @param col A character vector of hex colors or color names to use for the
#' stacked segments, bottom to top. Recycled if shorter than the number of
#' segments. Default is `NULL`, which automatically applies the SCI color palette.
#' @param greyscale A logical toggle switch replacing the color palette by a
#' sequential grey ramp, for print or black-and-white reproduction. Default is
#' `FALSE`. Ignored if `col` is supplied.
#' @param percent_labels A logical toggle switch enabling the percentage labels
#' printed inside the segments. Default is `TRUE`. Empty segments, and segments
#' too thin to hold a label, are left unlabelled.
#' @param col_percent A character value giving the color of the percentage
#' labels. Default is `NULL`, which picks black or white per segment, whichever
#' contrasts better with the fill.
#' @param cex_percent A numeric value specifying the font size of the percentage
#' labels. Default is `0.8`.
#' @param digits A numeric value indicating the number of decimal places shown
#' in the percentage labels. Default is `0`.
#' @param bar_width A numeric value giving the width of each bar in x-axis
#' units. Default is `NULL`, which fills about 70% of each time point with the
#' cluster of group bars.
#' @param cex_lab A numeric value specifying the font size of the main axis labels. Default is `1`.
#' @param cex_axis A numeric value specifying the font size of the axis tick marks. Default is `1`.
#' @param cex_n_patients A numeric value specifying the font size of the group
#' labels beneath the bars and of the table of patient numbers. Default is `0.7`.
#' @param risk_table A logical toggle switch enabling the table of patient
#' numbers (the denominator of each bar) beneath the x-axis. Default is `FALSE`.
#' @param missing_nr A numeric or character value indicating how zero-count patient
#' sizes on the x-axis should be displayed. Default is `0`. Can also be set to
#' `NA`, which leaves the cell blank.
#' @param rotate_labels A logical toggle switch rotating the x-axis tick labels
#' by 90 degrees. Default is `FALSE`.
#' @param mar_custom A numeric vector indicating the plot margins to be used in
#' the form `c(bottom, left, top, right)`. Default is `NULL`, which dynamically
#' calculates spacing for the labels, the table of patient numbers and the legend.
#' @param xlab_line A numeric value indicating the margin line offset for the x-axis label.
#' Default is `NULL`, which calculates an adaptive offset.
#' @param ylab_line A numeric value indicating the margin line offset for the y-axis label.
#' Default is `3.0`.
#' @param xlab_position Horizontal position of the row labels of the table of
#' patient numbers. Default is `NULL`, which aligns them just outside the plot region.
#' @param legend_position Position of the legend box. `"right"` (the default)
#' places it outside the plot region to the right of the bars; `"none"`
#' suppresses it. Standard base R keywords (e.g., `"topright"`) place it inside
#' the plot region, and a numeric pair `c(x, y)` gives explicit coordinates.
#' @param legend_title Title of the legend box. Default is `NULL` (no title).
#' @param legend_offset A numeric vector of length 2 specifying an `inset` to
#' nudge a keyword-placed legend. Default is `c(0, 0)`.
#' @param legend_cex A numeric value specifying the font and element scaling size
#' inside the legend box. Default is `1`.
#' @param legend_bty The type of box border to be drawn around the legend.
#' Allowed values are `"n"` (no border, the default) and `"o"` (standard box frame).
#' @param ... Further graphical parameters passed on to `plot()`.
#'
#' @return Invisibly, a list with `counts` and `percent`, arrays indexed by
#' segment, group and time point, and `n`, the group-by-time matrix of
#' denominators.
#'
#' @examples
#' library(summarySCI)
#' data("hemoglobin_data")
#' hemoglobin_data$hb_cat <- cut(hemoglobin_data$hb, c(-Inf, 120, 140, Inf),
#'                               labels = c("< 120", "120-140", "> 140"))
#' stackedbarplotSCI(data = hemoglobin_data,
#'                   var = hb_cat,
#'                   time = visit_nr,
#'                   time_labels = visit,
#'                   group = Arm,
#'                   group_names_short = c("A", "B"),
#'                   legend_title = "Hemoglobin",
#'                   risk_table = TRUE)
#'
#' # Several Yes/No columns, one bar per arm
#' set.seed(14)
#' ae <- data.frame(id = 1:50,
#'                  arm = sample(c("Arm A", "Arm B"), 50, replace = TRUE),
#'                  fatigue = sample(c("Yes", "No"), 50, replace = TRUE),
#'                  nausea  = sample(c("Yes", "No"), 50, replace = TRUE))
#' stackedbarplotSCI(ae, var = c("fatigue", "nausea"), group = arm, id = id,
#'                   scale = "count", legend_title = "Adverse events")
#'
#' @export
stackedbarplotSCI <- function(data, var, time = NULL, time_labels = NULL,
                              group = NULL, id = NULL,
                              group_names = NULL, group_names_short = NULL,
                              level_names = NULL,
                              scale = c("percent", "count"),
                              missing = FALSE, missing_text = "Missing",
                              main = "", xlab = NULL, ylab = NULL,
                              ylim = NULL, y_ticks = NULL,
                              col = NULL, greyscale = FALSE,
                              percent_labels = TRUE, col_percent = NULL,
                              cex_percent = 0.8, digits = 0, bar_width = NULL,
                              cex_lab = 1, cex_axis = 1, cex_n_patients = 0.7,
                              risk_table = FALSE, missing_nr = 0,
                              rotate_labels = FALSE,
                              mar_custom = NULL, xlab_line = NULL,
                              ylab_line = 3.0, xlab_position = NULL,
                              legend_position = "right", legend_title = NULL,
                              legend_offset = c(0, 0), legend_cex = 1,
                              legend_bty = "n", ...) {

  if (missing(data)) stop("'data' must be specified.")
  data <- as.data.frame(data)
  scale <- match.arg(scale)

  # Accept quoted ("x", c("x", "y")) and unquoted (x) column names; a bare
  # symbol errors on evaluation and is deparsed instead
  as_names <- function(sub, val_fun) {
    if (is.null(sub)) return(NULL)
    val <- tryCatch(val_fun(), error = function(e) NULL)
    if (is.character(val)) val else deparse(sub)
  }
  var_name   <- as_names(substitute(var),         function() var)
  time_name  <- as_names(substitute(time),        function() time)
  tlab_name  <- as_names(substitute(time_labels), function() time_labels)
  group_name <- as_names(substitute(group),       function() group)
  id_name    <- as_names(substitute(id),          function() id)

  missing_cols <- setdiff(c(var_name, time_name, tlab_name, group_name, id_name),
                          names(data))
  if (length(missing_cols) > 0)
    stop("Column(s) not found in 'data': ", paste(missing_cols, collapse = ", "), ".")

  keep <- rep(TRUE, nrow(data))
  if (!is.null(time_name))  keep <- keep & !is.na(data[[time_name]])
  if (!is.null(group_name)) keep <- keep & !is.na(data[[group_name]])
  data <- data[keep, , drop = FALSE]

  # --- Time points and groups as factors -------------------------------------
  as_time_factor <- function(x) {
    if (is.factor(x)) return(droplevels(x))
    if (is.numeric(x)) return(factor(x, levels = sort(unique(x))))
    x   <- as.character(x)
    u   <- unique(x)
    num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", u)))
    factor(x, levels = if (anyNA(num)) u else u[order(num)])
  }
  tt <- if (is.null(time_name)) factor(rep("1", nrow(data))) else
    as_time_factor(data[[time_name]])
  gg <- if (is.null(group_name)) factor(rep("All", nrow(data))) else
    factor(data[[group_name]])

  t_labels <- levels(tt)
  if (!is.null(tlab_name)) {
    t_labels <- as.character(tapply(as.character(data[[tlab_name]]), tt,
                                    function(z) z[1L]))
  }

  # --- Counts per segment, group and time point ------------------------------
  if (length(var_name) == 1L) {
    x   <- data[[var_name]]
    lev <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
    x   <- factor(as.character(x), levels = lev)
    if (isTRUE(missing) && anyNA(x)) {
      lev <- c(lev, missing_text)
      x   <- factor(ifelse(is.na(x), missing_text, as.character(x)), levels = lev)
    }
    counts <- unclass(table(x, gg, tt))
    n      <- apply(counts, c(2, 3), sum)
  } else {
    observed <- unique(unlist(lapply(data[var_name],
                                     function(z) as.character(unique(z)))))
    observed <- observed[!is.na(observed)]
    if (!all(observed %in% c("Yes", "No")))
      stop("When 'var' names several columns, all of them must be coded ",
           "'Yes' / 'No'. Found: ",
           paste(setdiff(observed, c("Yes", "No")), collapse = ", "), ".")

    counts <- array(0, dim = c(length(var_name), nlevels(gg), nlevels(tt)),
                    dimnames = list(var_name, levels(gg), levels(tt)))
    for (v in var_name) {
      yes <- !is.na(data[[v]]) & data[[v]] == "Yes"
      counts[v, , ] <- table(gg[yes], tt[yes])
    }
    n <- if (is.null(id_name)) unclass(table(gg, tt)) else
      tapply(data[[id_name]], list(gg, tt), function(z) length(unique(z)))
    n[is.na(n)] <- 0
  }
  names(dimnames(counts)) <- NULL
  n <- matrix(n, nrow = nlevels(gg), dimnames = list(levels(gg), levels(tt)))

  if (!is.null(level_names)) {
    if (length(level_names) != dim(counts)[1])
      stop("'level_names' must have one entry per segment (", dim(counts)[1], ").")
    dimnames(counts)[[1]] <- level_names
  }

  stackedbar_draw(counts, n, time_labels = t_labels,
                  group_names = group_names, group_names_short = group_names_short,
                  scale = scale, main = main, xlab = xlab, ylab = ylab,
                  ylim = ylim, y_ticks = y_ticks, col = col, greyscale = greyscale,
                  percent_labels = percent_labels, col_percent = col_percent,
                  cex_percent = cex_percent, digits = digits, bar_width = bar_width,
                  cex_lab = cex_lab, cex_axis = cex_axis,
                  cex_n_patients = cex_n_patients, risk_table = risk_table,
                  missing_nr = missing_nr, rotate_labels = rotate_labels,
                  mar_custom = mar_custom, xlab_line = xlab_line,
                  ylab_line = ylab_line, xlab_position = xlab_position,
                  legend_position = legend_position, legend_title = legend_title,
                  legend_offset = legend_offset, legend_cex = legend_cex,
                  legend_bty = legend_bty, ...)
}

#' Draw a stacked barplot from pre-computed counts
#'
#' Drawing back end shared by [stackedbarplotSCI()], which counts raw data, and
#' [plotByVisit()], which reads the counts off a [summaryByVisit()] table.
#' @inheritParams stackedbarplotSCI
#'
#' @param counts Numeric array of counts indexed by segment, group and time
#' point, with dimnames.
#' @param n Numeric group-by-time matrix of denominators for the percentages.
#'
#' @return Invisibly, a list with `counts`, `n` and `percent`.
#' @keywords internal
stackedbar_draw <- function(counts, n, time_labels = dimnames(counts)[[3]],
                            group_names = NULL, group_names_short = NULL,
                            scale = "percent", main = "", xlab = NULL,
                            ylab = NULL, ylim = NULL, y_ticks = NULL,
                            col = NULL, greyscale = FALSE,
                            percent_labels = TRUE, col_percent = NULL,
                            cex_percent = 0.8, digits = 0, bar_width = NULL,
                            cex_lab = 1, cex_axis = 1, cex_n_patients = 0.7,
                            risk_table = FALSE, missing_nr = 0,
                            rotate_labels = FALSE, mar_custom = NULL,
                            xlab_line = NULL, ylab_line = 3.0,
                            xlab_position = NULL,
                            legend_position = "right", legend_title = NULL,
                            legend_offset = c(0, 0), legend_cex = 1,
                            legend_bty = "n", ...) {

  n_lev  <- dim(counts)[1]
  n_grp  <- dim(counts)[2]
  n_time <- dim(counts)[3]
  levs   <- dimnames(counts)[[1]]

  groups <- dimnames(counts)[[2]]
  if (!is.null(group_names)) {
    if (length(group_names) != n_grp)
      stop("'group_names' must have one entry per group (", n_grp, ").")
    groups <- group_names
  }
  groups_short <- if (is.null(group_names_short)) groups else group_names_short

  percent <- sweep(counts, c(2, 3), n, "/") * 100
  percent[!is.finite(percent)] <- NA
  heights <- if (scale == "percent") percent else counts
  heights[is.na(heights)] <- 0

  # --- Colors ----------------------------------------------------------------
  if (is.null(col)) {
    col <- if (isTRUE(greyscale)) {
      paste0("gray", round(40 + (50 / n_lev) * seq_len(n_lev)))
    } else {
      sci_palette(n_lev)
    }
  }
  col <- rep(col, length.out = n_lev)
  txt_col <- if (is.null(col_percent)) {
    rgb <- grDevices::col2rgb(col) / 255
    ifelse(0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ] > 0.6,
           "black", "white")
  } else {
    rep(col_percent, length.out = n_lev)
  }

  # --- Bar positions: one cluster of group bars per time point ---------------
  show_grp <- n_grp > 1 && n_time > 1
  if (n_time == 1L) {
    x_at   <- matrix(seq_len(n_grp), n_grp, 1)
    tick_at <- seq_len(n_grp)
    tick_labels <- groups
    if (is.null(bar_width)) bar_width <- 0.6
  } else {
    shift  <- 0.8 / n_grp
    offs   <- (seq_len(n_grp) - (n_grp + 1) / 2) * shift
    x_at   <- outer(offs, seq_len(n_time), "+")
    tick_at <- seq_len(n_time)
    tick_labels <- time_labels
    if (is.null(bar_width)) bar_width <- 0.85 * shift
  }

  if (is.null(xlab)) xlab <- if (n_time > 1) "Time" else if (n_grp > 1) "Group" else ""
  if (is.null(ylab)) ylab <- if (scale == "percent") "Percentage of patients" else "Number of patients"

  tops <- apply(heights, c(2, 3), sum)
  if (is.null(ylim)) {
    top  <- if (scale == "percent") max(100, tops) else max(tops)
    ylim <- c(0, if (top > 0) top * 1.04 else 1)
  }

  # --- Plot region -----------------------------------------------------------
  label_line  <- if (show_grp) 0.8 else 0
  header_line <- (if (rotate_labels) 6 else 3) + label_line
  n_rows      <- if (n_time == 1L) 1 else n_grp
  show_legend <- !identical(legend_position, "none")
  outside     <- show_legend && identical(legend_position, "right")

  if (is.null(mar_custom)) {
    bottom <- (if (rotate_labels) 7 else 4) + label_line
    left   <- max(4.5, ylab_line + 1.5)
    if (risk_table) {
      bottom    <- bottom + n_rows + 2.5
      lab_chars <- max(nchar(c("No. of patients", groups_short)))
      left      <- max(left, 1.5 + 0.45 * lab_chars * cex_n_patients)
    }
    right <- if (outside) {
      2 + 0.5 * legend_cex * max(nchar(c(levs, legend_title))) + 2.5 * legend_cex
    } else 2
    mar_custom <- c(bottom, left, if (nzchar(main)) 3 else 2, right)
  }
  op <- graphics::par(mar = mar_custom)
  on.exit(graphics::par(op), add = TRUE)

  graphics::plot(NULL, xlim = c(0.5, max(tick_at) + 0.5), ylim = ylim,
                 yaxs = "i", main = main, xlab = "", ylab = "",
                 xaxt = "n", yaxt = "n", ...)
  usr <- graphics::par("usr")

  # --- Bars and percentage labels --------------------------------------------
  min_h <- graphics::strheight("0", cex = cex_percent) * 1.2
  for (j in seq_len(n_time)) {
    for (i in seq_len(n_grp)) {
      h <- heights[, i, j]
      if (sum(h) == 0) next
      top_y <- cumsum(h)
      bot_y <- top_y - h
      x0 <- x_at[i, j] - bar_width / 2
      x1 <- x_at[i, j] + bar_width / 2
      graphics::rect(x0, bot_y, x1, top_y, col = col, border = "white", lwd = 0.5)
      if (isTRUE(percent_labels)) {
        lab <- counts[, i, j] > 0 & h >= min_h
        if (any(lab)) {
          graphics::text(x_at[i, j], ((bot_y + top_y) / 2)[lab],
                         paste0(formatC(percent[lab, i, j], format = "f",
                                        digits = digits), "%"),
                         col = txt_col[lab], cex = cex_percent)
        }
      }
    }
  }
  graphics::box()

  # --- Axes ------------------------------------------------------------------
  if (is.null(y_ticks)) {
    graphics::axis(2, las = 1, cex.axis = cex_axis)
  } else {
    graphics::axis(2, at = y_ticks, las = 1, cex.axis = cex_axis)
  }
  graphics::axis(1, at = tick_at, labels = FALSE)
  graphics::axis(1, at = tick_at, labels = tick_labels, tick = FALSE,
                 line = label_line, cex.axis = cex_axis,
                 las = if (rotate_labels) 2 else 1)
  if (show_grp) {
    graphics::mtext(rep(groups_short, n_time), side = 1, line = 0.2,
                    at = as.vector(x_at), cex = cex_n_patients)
  }
  graphics::mtext(ylab, side = 2, line = ylab_line, cex = cex_lab)

  # --- Patient numbers: the denominator of each bar --------------------------
  if (risk_table) {
    shown <- matrix(as.character(n), nrow = n_grp)
    shown[n == 0] <- if (is.na(missing_nr)) "" else as.character(missing_nr)
    lab_at <- if (is.null(xlab_position)) usr[1] - 0.02 * diff(usr[1:2])
    else xlab_position

    graphics::mtext("No. of patients", side = 1, line = header_line, at = lab_at,
                    adj = 1, cex = cex_n_patients, font = 2, xpd = NA)
    if (n_time == 1L) {
      graphics::mtext(shown[, 1], side = 1, line = header_line + 1,
                      at = tick_at, cex = cex_n_patients)
    } else {
      for (i in seq_len(n_grp)) {
        ln <- header_line + i
        graphics::mtext(groups_short[i], side = 1, line = ln, at = lab_at,
                        adj = 1, cex = cex_n_patients, xpd = NA)
        graphics::mtext(shown[i, ], side = 1, line = ln, at = tick_at,
                        cex = cex_n_patients)
      }
    }
  }

  # --- x-axis label ----------------------------------------------------------
  if (is.null(xlab_line)) {
    xlab_line <- if (risk_table) header_line + n_rows + 1.5
    else (if (rotate_labels) 5.5 else 2.5) + label_line
  }
  graphics::mtext(xlab, side = 1, line = xlab_line, cex = cex_lab)

  # --- Legend: top segment listed first --------------------------------------
  if (show_legend) {
    legend_args <- list(legend = rev(levs), fill = rev(col), border = NA,
                        title = legend_title, title.adj = 0,
                        bty = legend_bty, cex = legend_cex,
                        bg = if (legend_bty == "n") NA else "white",
                        box.col = "grey70")
    if (outside) {
      do.call(graphics::legend,
              c(list(x = usr[2] + 0.02 * diff(usr[1:2]), y = usr[4], xpd = NA),
                legend_args))
    } else if (is.numeric(legend_position) && length(legend_position) == 2L) {
      do.call(graphics::legend,
              c(list(x = legend_position[1], y = legend_position[2]), legend_args))
    } else {
      do.call(graphics::legend,
              c(list(x = legend_position, inset = legend_offset), legend_args))
    }
  }

  invisible(list(counts = counts, n = n, percent = percent))
}

#' SCI color palette
#'
#' @param n Number of colors.
#' @return A character vector of `n` hex colors, light to dark.
#' @keywords internal
sci_palette <- function(n) {
  base <- c("#FFE3E3", "#F4B9C2", "#E4002B", "#961D13", "#401F28")
  switch(as.character(n),
         "1" = base[3],
         "2" = base[2:3],
         "3" = base[1:3],
         "4" = base[1:4],
         "5" = base,
         grDevices::colorRampPalette(base)(n))
}
