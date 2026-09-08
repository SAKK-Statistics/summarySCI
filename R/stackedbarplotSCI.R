#' Stacked bar chart of several Yes/No indicators, by group
#'
#' Generates a stacked bar chart of the number of patients answering `"Yes"` to
#' each of several binary indicator columns, optionally split into sub-groups
#' (e.g., treatment arms). Segments can be annotated with the percentage of
#' patients they represent.
#'
#' @param data A data frame or tibble containing the data to be plotted.
#' @param vars A character vector naming the columns in `data` to be stacked.
#' All of them must be coded `"Yes"` / `"No"` (missing values are allowed and
#' are counted as `"No"`).
#' @param id Name of the column in `data` identifying the patient. Used as the
#' denominator for the percentages (`n_distinct(id)` per group). Supports bare
#' or quoted column names.
#' @param group Name of the column in `data` containing the grouping variable
#' (e.g., treatment arms). Supports bare or quoted column names. Default is
#' `NULL`, which draws a single bar and suppresses the x-axis ticks and labels.
#' @param vars_names A character vector containing the names of the stacked
#' indicators to be displayed in the legend. The order must match `vars`.
#' Default is `NULL`, which falls back to the column names themselves.
#' @param group_names A character vector containing the names of the groups
#' to be displayed on the x-axis. The order must match `levels(factor(data$group))`.
#' Default is `NULL`, which falls back to the group names found in the data.
#' @param main Title of the plot. Default is `NULL` (no title).
#' @param xlab A label for the x-axis. Default is `NULL` (no label).
#' @param ylab A label for the y-axis. Default is `"Number of patients"`.
#' @param col A character vector of hex colors or color names to use for the
#' stacked segments. Recycled if shorter than `length(vars)`. Default is `NULL`,
#' which automatically applies the SCI color palette.
#' @param greyscale A logical toggle switch replacing the color palette by a
#' sequential grey ramp, for print or black-and-white reproduction. Default is
#' `FALSE`. When `TRUE`, the segment labels switch to black unless `col_percent`
#' is set explicitly. Ignored if `col` is supplied.
#' @param percent_labels A logical toggle switch enabling the percentage labels
#' printed inside the segments. Default is `TRUE`. Segments with a count of zero
#' are left unlabelled.
#' @param col_percent A character value giving the color of the percentage
#' labels. Default is `"white"`.
#' @param cex_percent A numeric value specifying the font size of the percentage
#' labels, relative to the ggplot2 default. Default is `1`.
#' @param digits A numeric value indicating the number of decimal places shown
#' in the percentage labels. Default is `0`.
#' @param legend_title Title of the legend box. Default is `NULL`, which falls
#' back to the ggplot2 default (the name of the fill variable).
#' @param legend_position Position of the legend. Standard ggplot2 keywords
#' (`"right"`, `"left"`, `"top"`, `"bottom"`, `"none"`) or a numeric coordinate
#' pair `c(x, y)` in normalised plot coordinates. Default is `"right"`.
#'
#' @return A `ggplot` object, so that further layers or themes can be added by
#' the caller. The summarised counts, denominators and percentages are available
#' as the `data` element of the returned object.
#'
#' @importFrom magrittr %>%
#' @export
stacked_barSCI <- function(data, vars, id, group = NULL,
                        vars_names = NULL, group_names = NULL,
                        main = NULL, xlab = NULL, ylab = "Number of patients",
                        col = NULL, greyscale = FALSE,
                        percent_labels = TRUE, col_percent = "white",
                        cex_percent = 1, digits = 0,
                        legend_title = NULL, legend_position = "right") {

  # Accept both quoted ("x") and unquoted (x) column names; a bare symbol
  # errors on evaluation and is deparsed instead
  as_name <- function(sub, val_fun) {
    if (is.null(sub)) return(NULL)
    val <- tryCatch(val_fun(), error = function(e) NULL)
    if (is.character(val) && length(val) == 1L) val else deparse(sub)
  }
  id_name    <- as_name(substitute(id),    function() id)
  group_name <- as_name(substitute(group), function() group)

  # --- Input checks ----------------------------------------------------------
  missing_cols <- setdiff(c(vars, id_name, group_name), names(data))
  if (length(missing_cols) > 0)
    stop("Column(s) not found in 'data': ", paste(missing_cols, collapse = ", "), ".")

  observed <- unique(unlist(lapply(data[vars], function(z) as.character(unique(z)))))
  observed <- observed[!is.na(observed)]
  if (!all(observed %in% c("Yes", "No")))
    stop("All columns in 'vars' must be coded 'Yes' / 'No'. Found: ",
         paste(setdiff(observed, c("Yes", "No")), collapse = ", "), ".")

  if (!is.null(vars_names) && length(vars_names) != length(vars))
    stop("'vars_names' must have one entry per variable in 'vars' (",
         length(vars), ").")

  # --- Reshape and summarise -------------------------------------------------
  dat_long <- data %>%
    dplyr::select(dplyr::all_of(c(id_name, group_name, vars))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(vars),
                        names_to = "stack", values_to = "present")

  dat_long$.id    <- dat_long[[id_name]]
  dat_long$.group <- if (is.null(group_name)) factor("") else
    factor(dat_long[[group_name]])

  dat_sum <- dat_long %>%
    dplyr::group_by(.group, stack) %>%
    dplyr::summarise(count   = sum(present == "Yes", na.rm = TRUE),
                     total   = dplyr::n_distinct(.id),
                     percent = 100 * count / total,
                     .groups = "drop")

  # Keep the stacking order given in 'vars' rather than alphabetical order
  dat_sum$stack <- factor(dat_sum$stack, levels = vars,
                          labels = if (is.null(vars_names)) vars else vars_names)

  if (!is.null(group_names)) {
    if (length(group_names) != nlevels(dat_sum$.group))
      stop("'group_names' must have one entry per group (",
           nlevels(dat_sum$.group), ").")
    levels(dat_sum$.group) <- group_names
  }

  # --- Colors ----------------------------------------------------------------
  n <- length(vars)
  if (is.null(col)) {
    if (isTRUE(greyscale)) {
      col <- paste0("gray", round(40 + (50 / n) * seq_len(n), 0))
      if (missing(col_percent)) col_percent <- "black"
    } else {
      col <- colourSCI(n = n)
    }
  }
  col <- rep(col, length.out = n)

  # --- Plot ------------------------------------------------------------------
  stacked_plt <- ggplot2::ggplot(
    dat_sum, ggplot2::aes(x = .group, y = count, fill = stack)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::labs(title = main, x = xlab, y = ylab, fill = legend_title) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position  = legend_position)

  if (is.null(group_name)) {
    stacked_plt <- stacked_plt +
      ggplot2::theme(axis.text.x  = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
  }

  if (isTRUE(percent_labels)) {
    stacked_plt <- stacked_plt +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(count == 0, "",
                                    paste0(round(percent, digits), "%"))),
        colour   = col_percent,
        size     = 3.88 * cex_percent,
        position = ggplot2::position_stack(vjust = 0.5))
  }

  stacked_plt
}

utils::globalVariables(c(".id", ".group", "stack", "present", "count",
                         "total", "percent"))
