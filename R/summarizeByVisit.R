#' Summarize data by visit as a table and plots
#'
#' Produces the table of [summaryByVisit()] and one plot per variable (or
#' either of them alone, see `type`), the plots
#' over the visits: a line plot or boxplots ([boxplotSCI()]) for continuous
#' variables, and stacked barplots for categorical ones. The type of each plot
#' follows the type the variable has in the table.
#'
#' The line plots and stacked bars are drawn from the numbers printed in the
#' table, as [plotByVisit()] does, so that they show exactly what the table
#' shows (with its rounding):
#' * `stat_cont` sets what the line plot shows: `"median_range"` the median with
#'   minimum and maximum, `"median_IQR"` the median with the quartiles,
#'   `"mean_sd"` / `"mean_se"` the mean +/- one standard deviation / standard
#'   error, and `"geomMean_sd"` the geometric mean divided and multiplied by
#'   the geometric standard deviation;
#' * the variable labels become the axis or legend titles, the column headers
#'   the group names, and the N columns the numbers of the risk table;
#' * `missing` / `missing_percent` decide whether missing values form a
#'   segment of their own (as they are counted in the table's percentages),
#'   and `stat_cat = "n"` shows counts instead of percentages;
#' * `continuous_as_categorical` variables are plotted as stacked bars, and
#'   with `overall = TRUE` the overall column is plotted as one more group.
#'
#' Boxplots show more than the table holds, and are drawn from the data: from
#' the rows the table summarizes, with the box at the quartiles of the table
#' (see [boxplotSCI()]).
#'
#' The visits appear in the order of the table. If `order` is given, its values
#' also set the spacing of the visits in the line plots.
#'
#' @inheritParams summaryByVisit
#' @inheritParams summarizeSCI
#' @param plot_cont The plot for continuous variables: `"lineplot"` (the
#' default) or `"boxplot"`.
#' @param plot_args A named list of further arguments for the plotting
#' functions, e.g. `list(risk_table = TRUE, dodge = 0.1)`. Each plot receives
#' the arguments its function has, as in [lineplotSCI()], [boxplotSCI()] and
#' [stackedbarplotSCI()], except those that compute the numbers (such as
#' `stat` or `interval`), which come from the table; an argument no plot uses
#' gives a warning. For
#' example, `stat_footnote = TRUE` adds a footnote saying what each plot shows:
#' for the line plot the statistic of `stat_cont` (e.g. "Median (Q1, Q3)").
#' @param ... Further arguments passed on to [summaryByVisit()], e.g.
#' `digits_cont`, `add_n` or `overall`.
#'
#' @return An object of class `summarizeByVisitSCI` (and `summarizeSCI`), a
#' list with `table` and `plots`; see [summarizeSCI()].
#'
#' @examples
#' library(summarySCI)
#' data("hemoglobin_data")
#' hb <- subset(hemoglobin_data, visit_nr <= 3)   # first 3 cycles, to keep it short
#' hb$hb_cat <- cut(hb$hb, c(-Inf, 120, 140, Inf),
#'                  labels = c("< 120", "120-140", "> 140"))
#' res <- summarizeByVisitSCI(hb, vars = c("hb", "hb_cat"),
#'                            group = "Arm", visit = "visit", order = "visit_nr",
#'                            stat_cont = "median_IQR")
#' res$plots$hb
#' res$plots$hb_cat
#'
#' @export
summarizeByVisitSCI <- function(data, vars = NULL, group = NULL, labels = NULL,
                                stat_cont = "median_range",
                                stat_cat = "n_percent",
                                visit = "visit", order = NULL, visitgroup = NULL,
                                missing = TRUE, missing_percent = TRUE,
                                missing_text = "Missing",
                                continuous_as_categorical = NULL,
                                type = c("both", "table", "plot"),
                                plot_cont = c("lineplot", "boxplot"),
                                plot_args = list(),
                                as_flex_table = TRUE, border = TRUE,
                                word_output = FALSE,
                                file_path = paste0("SummaryByVisit_",
                                                   format(Sys.Date(), "%Y%m%d"),
                                                   ".docx"),
                                ...) {

  if (missing(data)) stop("'data' must be specified.")
  data <- as.data.frame(data)
  type      <- match.arg(type)
  plot_cont <- match.arg(plot_cont)
  check_wrapper_args(plot_args, type, word_output)

  tbl <- summaryByVisit(data, vars = vars, group = group, labels = labels,
                        stat_cont = stat_cont, stat_cat = stat_cat,
                        visit = visit, order = order, visitgroup = visitgroup,
                        missing = missing, missing_percent = missing_percent,
                        missing_text = missing_text,
                        continuous_as_categorical = continuous_as_categorical,
                        ..., as_flex_table = FALSE, word_output = FALSE)

  plots <- list()
  if (type != "table") {
    # type and label of each variable, in the order of the table
    sm    <- parse_summary_by_visit(tbl, stat_cont = stat_cont, strict = FALSE)
    labs  <- unique(sm$variable)
    types <- sm$var_type[match(labs, sm$variable)]
    if (is.null(vars)) vars <- setdiff(names(data), c(group, visit))  # as summaryByVisit
    if (length(vars) != length(labs))
      stop("Could not match the variables of the table to 'vars'.")

    # the plot_args the drawing functions take (not those computing numbers)
    common <- function(f, draw) setdiff(intersect(names(formals(f)),
                                                  names(formals(draw))), "visit")
    line_args <- common(lineplotSCI, lineplot_draw)
    bar_args  <- c(common(stackedbarplotSCI, stackedbar_draw), "missing")

    overall <- isTRUE(list(...)$overall)
    specs <- lapply(seq_along(vars), function(i) {
      if (types[i] == "categorical") {
        return(list(fun = plotByVisit, name = "stacked barplot", data = tbl,
                    accepts = bar_args,
                    args = list(var = i, missing_text = missing_text,
                                missing = isTRUE(missing) && !isFALSE(missing_percent),
                                scale = if (stat_cat == "n") "count" else "percent")))
      }
      # the table's visits of this variable, and the visit of each row (NA
      # for rows the table leaves out, e.g. without a visit label)
      visits <- unique(sm$visit[sm$variable == labs[i]])
      pos    <- match(as.character(data[[visit]]), visits)

      if (plot_cont == "boxplot") {
        keep <- !is.na(pos)
        if (!is.null(group)) keep <- keep & !is.na(data[[group]])
        bdata <- data[keep, , drop = FALSE]
        bdata$.visit_id    <- pos[keep]
        bdata$.visit_label <- visits[pos[keep]]
        # the overall column of the table, as one more group
        if (!is.null(group) && overall) {
          g     <- factor(bdata[[group]])
          bdata <- rbind(bdata, bdata)
          bdata[[group]] <- factor(c(as.character(g), rep("Overall", length(g))),
                                   levels = c(levels(g), "Overall"))
        }
        return(list(fun = boxplotSCI, name = "boxplot", data = bdata,
                    args = list(var = vars[i], order = ".visit_id",
                                visit = ".visit_label", group = group,
                                ylab = labs[i], xlab = "Visit")))
      }
      # the visits spaced by 'order' (evenly, if a visit has no value of it)
      xs <- if (!is.null(order)) vapply(seq_along(visits), function(k)
        suppressWarnings(min(data[[order]][pos %in% k], na.rm = TRUE)), numeric(1))
      if (!all(is.finite(xs)) || is.unsorted(xs, strictly = TRUE)) xs <- NULL
      list(fun = plotByVisit, name = "line plot", data = tbl, accepts = line_args,
           args = list(var = i, stat_cont = stat_cont, xs = xs))
    })
    names(specs) <- vars
    plots <- build_plots(specs, data, plot_args)
  }

  wrapper_result(tbl, plots, type, as_flex_table, border,
                 c("summarizeByVisitSCI", "summarizeSCI"),
                 word_output = word_output, file_path = file_path)
}
