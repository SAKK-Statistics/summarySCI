#' Summarize data as a table and plots
#'
#' Produces the summary table of [summaryTable()] and one plot per variable,
#' or either of them alone (see `type`): a boxplot for continuous variables
#' ([boxplotSCI()]) and a
#' stacked barplot for categorical and dichotomous ones
#' ([stackedbarplotSCI()]). The type of each plot follows the type the
#' variable has in the table, so that table and plots always agree.
#'
#' The settings of the table carry over to the plots: the variable labels
#' become the axis or legend titles, `missing` / `missing_percent` decide
#' whether missing values form a segment of their own (as they are counted in
#' the table's percentages), and `stat_cat = "n"` shows counts instead of
#' percentages. Options that only concern the plots go into `plot_args`.
#'
#' @inheritParams summaryTable
#' @param type What to produce: `"both"` (the default) the table and a plot per
#' variable, `"table"` the table alone, or `"plot"` the plots alone. The table
#' is built in every case, since it decides the type of each plot.
#' @param plot_args A named list of further arguments for the plotting
#' functions, e.g. `list(risk_table = TRUE, col = c("grey", "black"))`. Each
#' plot receives the arguments its function has.
#' @param ... Further arguments passed on to [summaryTable()], e.g. `test`,
#' `ci`, `overall` or `digits_cont`.
#'
#' @return An object of class `summarizeSCI`, a list with
#' * `table`: the table, a flextable or (with `as_flex_table = FALSE`) a
#'   gtsummary object; `NULL` with `type = "plot"`. With `word_output = TRUE`
#'   it is also saved to `file_path`, as [summaryTable()] does;
#' * `plots`: a named list with one plot per variable, e.g. `res$plots$hb`;
#'   empty with `type = "table"`. Printing a plot draws it; [save_plots()]
#'   writes them to files.
#'
#' Printing the result shows the table and lists the plots.
#'
#' @examples
#' library(summarySCI)
#' data("hemoglobin_data")
#' d <- hemoglobin_data[hemoglobin_data$visit_nr == 2, ]
#' d$hb_cat <- cut(d$hb, c(-Inf, 120, 140, Inf),
#'                 labels = c("< 120", "120-140", "> 140"))
#' res <- summarizeSCI(d, vars = c("hb", "hb_cat"), group = "Arm")
#' res$table
#' res$plots$hb
#' res$plots$hb_cat
#'
#' # the plots alone
#' plots <- summarizeSCI(d, vars = c("hb", "hb_cat"), group = "Arm",
#'                       type = "plot")$plots
#' \dontrun{
#' save_plots(res, file_name = "fig", file_path = "figures")
#' }
#'
#' @export
summarizeSCI <- function(data, vars = NULL, group = NULL, labels = NULL,
                         stat_cont = "median_range", stat_cat = "n_percent",
                         missing = TRUE, missing_percent = TRUE,
                         missing_text = "Missing",
                         type = c("both", "table", "plot"), plot_args = list(),
                         as_flex_table = TRUE, border = TRUE,
                         word_output = FALSE,
                         file_path = paste0("SummaryTable_",
                                            format(Sys.Date(), "%Y%m%d"),
                                            ".docx"),
                         ...) {

  if (missing(data)) stop("'data' must be specified.")
  data <- as.data.frame(data)
  type <- match.arg(type)
  check_wrapper_args(plot_args, type, word_output)

  tbl <- summaryTable(data, vars = vars, group = group, labels = labels,
                      stat_cont = stat_cont, stat_cat = stat_cat,
                      missing = missing, missing_percent = missing_percent,
                      missing_text = missing_text, ...,
                      as_flex_table = FALSE, word_output = FALSE)

  plots <- list()
  if (type != "table") {
    # type and label of each variable, as the table has them
    tb    <- as.data.frame(tbl$table_body)
    rows  <- tb$row_type == "label" & !duplicated(tb$variable)
    types <- stats::setNames(tb$var_type[rows], tb$variable[rows])
    labs  <- stats::setNames(tb$label[rows], tb$variable[rows])

    specs <- lapply(stats::setNames(names(types), names(types)), function(v) {
      if (types[[v]] == "continuous") {
        list(fun = boxplotSCI, name = "boxplot",
             args = list(var = v, group = group, ylab = labs[[v]],
                         xlab = if (is.null(group)) "" else NULL))
      } else {
        bar_spec(v, labs[[v]], group, stat_cat, missing, missing_percent,
                 missing_text)
      }
    })
    plots <- build_plots(specs, data, plot_args)
  }

  wrapper_result(tbl, plots, type, as_flex_table, border, "summarizeSCI",
                 word_output = word_output, file_path = file_path)
}

#' @export
print.summarizeSCI <- function(x, ...) {
  if (!is.null(x$table)) print(x$table)
  if (length(x$plots) > 0) {
    types <- vapply(x$plots, function(p) p$type, character(1))
    cat("Plots - draw with x$plots$<name>, save with save_plots(x):\n")
    cat(paste0("  ", format(names(types)), "  ", types), sep = "\n")
  }
  invisible(x)
}
