#' Summarize data as a table or a plot
#'
#' Wrapper around [summaryTable()], [boxplotSCI()], [lineplotSCI()] and
#' [stackedbarplotSCI()]. Arguments in `...` are passed on unchanged, so they
#' must be those of the function selected by `type` (e.g. `vars` for tables,
#' `var` for plots).
#'
#' @param type Which summary to produce: `"table"`, `"boxplot"`, `"lineplot"`
#' or `"barplot"`.
#' @inheritDotParams summaryTable
#' @inheritDotParams boxplotSCI
#' @inheritDotParams lineplotSCI
#' @inheritDotParams stackedbarplotSCI
#' @export
summarizeSCI <- function(type = c("table", "boxplot", "lineplot", "barplot"), ...) {
  type <- match.arg(type)
  switch(type,
         table    = summaryTable(...),
         boxplot  = boxplotSCI(...),
         lineplot = lineplotSCI(...),
         barplot  = stackedbarplotSCI(...)
  )
}
