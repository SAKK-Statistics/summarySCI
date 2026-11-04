#' Summarize data by visit as a table and plots
#'
#' Produces the table of [summaryByVisit()] and one plot per variable (or
#' either of them alone, see `type`), the plots
#' over the visits: a line plot ([lineplotSCI()]) or boxplots
#' ([boxplotSCI()]) for continuous variables, and stacked barplots
#' ([stackedbarplotSCI()]) for categorical ones. The type of each plot follows
#' the type the variable has in the table, so that table and plots agree.
#'
#' The settings of the table carry over to the plots:
#' * `stat_cont` sets what the line plot shows: `"median_range"` the median with
#'   minimum and maximum, `"median_IQR"` the median with the quartiles,
#'   `"mean_sd"` / `"mean_se"` the mean +/- one standard deviation / standard
#'   error, and `"geomMean_sd"` the geometric mean alone;
#' * the variable labels become the axis or legend titles;
#' * `missing` / `missing_percent` decide whether missing values form a
#'   segment of their own (as they are counted in the table's percentages),
#'   and `stat_cat = "n"` shows counts instead of percentages;
#' * `continuous_as_categorical` variables are plotted as stacked bars.
#'
#' The visits appear in the order of the table. If `order` is given, its values
#' also set the spacing of the visits in the line plots.
#'
#' @inheritParams summaryByVisit
#' @inheritParams summarizeSCI
#' @param plot_cont The plot for continuous variables: `"lineplot"` (the
#' default) or `"boxplot"`.
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
                                as_flex_table = TRUE, border = TRUE, ...) {

  if (missing(data)) stop("'data' must be specified.")
  data <- as.data.frame(data)
  check_wrapper_args(list(...), plot_args, "summaryByVisit")
  type      <- match.arg(type)
  plot_cont <- match.arg(plot_cont)

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
    sm <- parse_summary_by_visit(tbl, stat_cont = stat_cont, strict = FALSE)
    labs  <- unique(sm$variable)
    types <- sm$var_type[match(labs, sm$variable)]
    if (is.null(vars)) vars <- setdiff(names(data), c(group, visit))  # as summaryByVisit
    if (length(vars) != length(labs))
      stop("Could not match the variables of the table to 'vars'.")

    # the data as tabulated: continuous_as_categorical variables as factors
    pdata <- data
    if (!is.null(continuous_as_categorical))
      pdata[continuous_as_categorical] <- lapply(pdata[continuous_as_categorical],
                                                 as.factor)

    # time axis: the 'order' column, or else the visits' position in the table
    order_col <- order
    if (is.null(order_col)) {
      visits <- unique(sm$visit[base::order(match(sm$variable, labs), sm$visit_id)])
      order_col <- ".visit_order"
      pdata[[order_col]] <- match(as.character(pdata[[visit]]), visits)
    }

    specs <- lapply(seq_along(vars), function(i) {
      v <- vars[i]
      if (types[i] == "categorical") {
        return(bar_spec(v, labs[i], group, stat_cat, missing, missing_percent,
                        missing_text, order = order_col, visit = visit,
                        xlab = "Visit"))
      }
      if (plot_cont == "boxplot") {
        return(list(fun = boxplotSCI, name = "boxplot",
                    args = list(var = v, order = order_col, visit = visit,
                                group = group, ylab = labs[i], xlab = "Visit")))
      }
      # the line plot shows the statistic of the table
      st <- switch(stat_cont,
                   median_range = c("median", "range"),
                   median_IQR   = c("median", "iqr"),
                   mean_sd      = c("mean", "sd"),
                   mean_se      = c("mean", "se"),
                   geomMean_sd  = c("geomean", "none"))
      list(fun = lineplotSCI, name = "line plot",
           args = list(var = v, order = order_col, group = group, visit = visit,
                       stat = st[1], interval = st[2],
                       ylab = labs[i], xlab = "Visit"))
    })
    names(specs) <- vars
    plots <- build_plots(specs, pdata, plot_args)
  }

  wrapper_result(tbl, plots, type, as_flex_table, border,
                 c("summarizeByVisitSCI", "summarizeSCI"))
}
