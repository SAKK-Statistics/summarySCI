#' Boxplot over time
#'
#' Generates boxplots over time for one or multiple sub-groups.
#'
#' @param data A data frame or tibble containing the data to be plotted.
#' @param var Name of the column in `data` indicating the measurement to be
#' plotted. Supports bare or quoted column names.
#' @param time Name of the column in `data` containing the time points of interest.
#' The time points can be either numeric, character, or a factor.
#' If the time points are factors, the factor levels must be in the correct
#' chronological order for plotting. If left empty, the function will assume
#' a single time point. If character, they need to be able to be ordered by
#' numerical logic.
#' @param time_labels Name of the column in `data` containing descriptive labels
#' for the time points. Optional.
#' @param group Name of the column in `data` containing the grouping variables
#' (e.g., treatment arms). If left empty, the function will assume a single group.
#' @param group_names A character vector containing the names of the groups
#' to be displayed in the legend. The order must match `levels(factor(data$group))`.
#' Default is `NULL`, which falls back to the group names found in the data.
#' @param group_names_short A character vector containing short names or abbreviations
#' for the groups, to be displayed below the x-axis near the patient counts.
#' Optional. Defaults to `group_names`.
#' @param main Title of the plot. Defaults to `""` (no title).
#' @param xlab A label for the x-axis. Default is `"Time"` if there are multiple
#' time points, otherwise defaults to `"Group"`.
#' @param ylab A label for the y-axis. Default is `"Variable"`.
#' @param ylim The y-axis limits of the plot as a numeric vector of length 2.
#' Default is `NULL`, which adds a 10% padding buffer above and below the data range.
#' @param y_ticks A numeric vector specifying the explicit tick positions on the
#' y-axis. Default is `NULL`, which calculates 5 evenly spaced ticks across the data range.
#' @param col A character vector of hex colors or color names to use for the boxes.
#' Default is `NULL`, which automatically applies the SCI color palette.
#' @param cex_lab A numeric value specifying the font size of the main axis labels. Default is `1`.
#' @param cex_axis A numeric value specifying the font size of the axis tick marks. Default is `1`.
#' @param cex_n_patients A numeric value specifying the font size of the sample size
#' labels and numbers displayed beneath the horizontal axis. Default is `0.7`.
#' @param boxwex Scale factor for the width of the boxes. Default is `NULL`, which
#' scales the box flexibly to the size of total timepoints and groups.
#' @param shift A numeric value indicating the distance between adjacent boxes
#' within a timepoint cluster. Default is `NULL`, which dynamically sets spacing to
#' `boxwex * 1.15`.
#' @param mar_custom A numeric vector indicating the plot margins to be used in
#' the form `c(bottom, left, top, right)`. Default is `NULL`, which dynamically
#' calculates spacing so that stacked sample-size rows are never clipped.
#' @param xlab_line A numeric value indicating the margin line offset for the x-axis label.
#' Default is `NULL`, which calculates an adaptive offset based on the number of groups.
#' @param ylab_line A numeric value indicating the margin line offset for the y-axis label.
#' Default is `3.0`.
#' @param xlab_position Horizontal position coordinate adjustment for the sample size
#' group headers down the left-hand margin. Default is `NULL`, which auto-aligns them.
#' @param lines A numeric vector indicating the vertical coordinates where horizontal
#' reference lines will be plotted. Optional.
#' @param lty A numeric vector of length 1 or of the same length as `lines`
#' indicating the line type to be plotted. Default is `1` (solid).
#' @param col_lines A character vector of length 1 or of the same length as
#' `lines` indicating the color of the reference lines. Default is `"grey"`.
#' @param legend_position Position of the legend box. Options include standard base R
#' keywords (e.g., `"topright"`, `"topleft"`, `"bottom"`) or manual numeric coordinate pairs
#' `c(x, y)`. Default is `"topright"`.
#' @param legend_offset A numeric vector of length 2 specifying an `inset` to nudge the
#' legend position away from plot elements. Default is `c(0, 0)`.
#' @param legend_cex A numeric value specifying the font and element scaling size
#' inside the legend box. Default is `1`.
#' @param legend_bty The type of box border to be drawn around the legend.
#' Allowed values are `"n"` (transparent/no border, the default) and `"o"` (standard box frame).
#' @param missing_nr A numeric or character value indicating how zero-count patient
#' sizes on the x-axis should be displayed. Default is `0`. Can also be set to `NA`.
#' @param color_time A logical toggle switch enabling legacy timepoint colorization. Supports
#' legacy use, where, for a single timepoint, multiple groups were used as "timepoint" variable.
#' If set to `TRUE`, it treats timepoints as individual categories,applying sequential coloring
#' and rendering a matching tracking legend. Default is `FALSE`.
#'
#' @examples
#' library(summarySCI)
#' data("hemoglobin_data")
#' boxplotSCI(data = hemoglobin_data,
#'                  var = hb,
#'                   time = visit_nr,
#'                   group = Arm,
#'                   group_names = c("Treatment (Arm A)", "Control (Arm B)"),
#'                   group_names_short = c("A", "B"),
#'                   # Title
#'                   main = "Hemoglobin over time",
#'                   # Col of boxes
#'                   col = c("lightblue", "orange"),
#'                   # X axis label
#'                   xlab = "Visits",
#'                   # X axis position
#'                   xlab_position = 0,
#'                   # Y axis label
#'                   ylab = "Score",
#'                   lines = c(90, 120, 160),
#'                   # Col of lines
#'                   col_lines = c("red", "grey", "red"),
#'                   # Type of lines
#'                   lty = c(2, 3, 2)
#' )
#'
#' @import stringr
#' @export

boxplotSCI <- function(data,
                              var,
                              time = NULL,
                              time_labels = NULL,
                              group = NULL,
                              group_names = NULL,
                              group_names_short = NULL,
                              main = "",
                              xlab = NULL,
                              ylab = "Variable",
                              ylim = NULL,
                              y_ticks = NULL,
                              col = NULL,
                              cex_lab = 1,
                              cex_axis = 1,
                              cex_n_patients = 0.7,
                              boxwex = NULL,
                              shift = NULL,
                              mar_custom = NULL,
                              xlab_line = NULL,
                              ylab_line = 3.0,
                              xlab_position = NULL,
                              lines = NULL,
                              lty = NULL,
                              col_lines = NULL,
                              legend_position = "topright",
                              legend_offset = c(0, 0),
                              legend_cex = 1,
                              legend_bty = "n",
                              missing_nr = 0,
                              color_time = FALSE) {

  # 1. Checks, input validation and graphical parameters
  if (missing(data)) stop("'data' must be specified.")
  data <- as.data.frame(data)

  # Unified parser
  get_col_name <- function(x_expr) {
    if (is.null(x_expr)) return(NULL)
    name <- deparse(x_expr)
    name <- gsub('^"|"$', '', name)
    if (name %in% colnames(data)) return(name)
    return(NULL)
  }

  var         <- get_col_name(substitute(var))
  group       <- get_col_name(substitute(group))
  time        <- get_col_name(substitute(time))
  time_labels <- get_col_name(substitute(time_labels))

  if (is.null(var)) stop("Specified target variable 'var' column not found.")

  group_number <- 1
  group_levels <- NULL
  if (!is.null(group)) {
    data[[group]] <- factor(data[[group]])
    group_levels  <- levels(data[[group]])
    group_number  <- length(group_levels)
  }

  if (is.null(group_names)) group_names <- group_levels
  if (is.null(group_names_short)) group_names_short <- group_names

  if (!is.null(time)) {
    if (is.character(data[[time]])) {
      data[[time]] <- factor(data[[time]], levels = unique(data[[time]]))
    }
    if (is.factor(data[[time]])) data[[time]] <- droplevels(data[[time]])
    master_timepoints <- sort(unique(data[[time]]))
  } else {
    master_timepoints <- 1
  }
  n_timepoints <- length(master_timepoints)

  # Design and environment configurations
  if (is.null(xlab)) xlab <- ifelse(is.null(time) || n_timepoints == 1, "Group", "Time")

  # Dynamic layout tracking
  if (is.null(xlab_line)) {
    if (is.null(time) || n_timepoints == 1) {
      xlab_line <- 3.0
    } else {
      xlab_line <- 2 + (group_number - 1) * 0.65 + 1.2
    }
  }

  # SCI color palette
  palette_n <- if (color_time && group_number == 1) n_timepoints else group_number

  if (is.null(col)) {
    if (palette_n <= 5) {
      col <- switch(as.character(palette_n),
                    "1" = "#E4002B",
                    "2" = c("#F4B9C2", "#E4002B"),
                    "3" = c("#FFE3E3", "#F4B9C2", "#E4002B"),
                    "4" = c("#FFE3E3", "#F4B9C2", "#E4002B", "#961D13"),
                    "5" = c("#FFE3E3", "#F4B9C2", "#E4002B", "#961D13", "#401F28")
      )
    } else {
      col <- colorRampPalette(c("#FFE3E3", "#F4B9C2", "#E4002B", "#961D13", "#401F28"))(palette_n)
    }
  }

  if (is.null(ylim)) {
    range_m <- range(data[[var]], na.rm = TRUE)
    ylim <- c(range_m[1] - 0.1 * abs(range_m[1]), range_m[2] + 0.1 * abs(range_m[2]))
  }
  if (is.null(y_ticks)) y_ticks <- seq(floor(ylim[1]), ceiling(ylim[2]), length.out = 5)

  # Flexible Boxwex calculation
  if (is.null(boxwex)) {
    if (is.null(time) || n_timepoints == 1) {
      boxwex <- 0.65 - 0.15 * (min(group_number, 12) / 12)
    } else {
      if (group_number == 1) {
        boxwex <- 0.65 - 0.15 * (min(n_timepoints, 12) / 12)
      } else {
        target_cluster_width <- 0.85 - 0.15 * (min(n_timepoints, 12) / 12)
        boxwex <- target_cluster_width / (1.15 * (group_number - 1) + 1)
      }
    }
  }
  if (is.null(shift))  shift  <- boxwex * 1.15

  # Dynamic Margins calculation
  if (is.null(mar_custom)) {
    if (is.null(time) || n_timepoints == 1) {
      calculated_bottom <- 4.0
      left_margin       <- 4.5
    } else {
      base_bottom       <- if (n_timepoints > 8) 6.5 else 5.0
      calculated_bottom <- base_bottom + (group_number - 1) * 0.65
      left_margin       <- if (n_timepoints > 8) 5.5 else 4.5
    }
    current_margins <- c(calculated_bottom, left_margin, 4, 2) + 0.1
  } else {
    current_margins <- mar_custom
  }
  par(mar = current_margins)

  group_offsets <- (1:group_number - (group_number + 1) / 2) * shift

  # 2. Rendering loop

  if (is.null(time) || n_timepoints == 1) {
    boxplot(data[[var]] ~ data[[group]], xlab = "", ylab = "", col = col,
            xaxt = "n", yaxt = "n", ylim = ylim, boxwex = boxwex)

    axis(side = 1, at = 1:group_number, labels = group_names, cex.axis = cex_axis)
  } else {
    for (g in 1:group_number) {
      g_subset <- if (is.null(group)) data else data[data[[group]] == group_levels[g], ]
      at_positions <- (1:n_timepoints) + group_offsets[g]

      current_box_col <- if (color_time && group_number == 1) col else col[g]

      boxplot(g_subset[[var]] ~ factor(g_subset[[time]], levels = master_timepoints),
              col = current_box_col, at = at_positions, add = (g > 1),
              xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = ylim, boxwex = boxwex,
              xlim = c(0.5, n_timepoints + 0.5))

      if (nrow(g_subset) > 0) {
        n_counts <- table(factor(g_subset[[time]][!is.na(g_subset[[var]])], levels = master_timepoints))
        n_counts[n_counts == 0] <- missing_nr
      } else {
        n_counts <- rep(missing_nr, n_timepoints)
      }

      current_line <- 2 + (g - 1) * 0.65
      mtext(text = as.vector(n_counts), side = 1, line = current_line, at = 1:n_timepoints, cex = cex_n_patients)

      if (group_number > 1) {
        if (is.null(xlab_position)) xlab_position <- par("usr")[1] - 0.03 * (par("usr")[2] - par("usr")[1])
        mtext(text = paste("#", group_names_short[g]), side = 1, line = current_line, at = xlab_position, cex = cex_n_patients, adj = 1)
      }
    }

    x_axis_labels <- if (is.null(time_labels)) master_timepoints else unique(data[order(data[[time]]), time_labels])
    axis(side = 1, at = 1:n_timepoints, labels = x_axis_labels, cex.axis = cex_axis)
  }

  # 3. Margin texts, annotations and legend
  axis(2, at = y_ticks, labels = y_ticks, cex.axis = cex_axis)

  title(main = main)
  title(xlab = xlab, line = xlab_line, cex.lab = cex_lab)
  title(ylab = ylab, line = ylab_line, cex.lab = cex_lab)

  # Show legend if multiple groups exist OR if there are multiple timepoints to track
  show_legend <- group_number > 1 || n_timepoints > 1

  if (show_legend) {
    if (group_number == 1 && color_time) {
      # Legacy Mode active: display distinct timepoint steps
      legend_labels <- if (is.null(time_labels)) master_timepoints else unique(data[order(data[[time]]), time_labels])
      legend_col    <- col
    } else if (group_number == 1 && !color_time) {
      # Standard Single Group Longitudinal: display the cohort name or fall back to the tested variable name
      legend_labels <- if (!is.null(group_names)) group_names else var
      legend_col    <- col[1]
    } else {
      # Standard Multi-group setup
      legend_labels <- group_names
      legend_col    <- col
    }

    if (length(legend_position) == 1 && is.character(legend_position)) {
      legend(x = legend_position, inset = legend_offset, legend = legend_labels,
             fill = legend_col, cex = legend_cex, bty = legend_bty)
    } else {
      legend(x = legend_position[1] + legend_offset[1],
             y = legend_position[2] + legend_offset[2],
             legend = legend_labels, fill = legend_col, cex = legend_cex, bty = legend_bty)
    }
  }

  if (!is.null(lines)) {
    if (is.null(lty)) lty <- 1
    if (is.null(col_lines)) col_lines <- "grey"
    abline(h = lines, lty = lty, col = col_lines)
  }
}
