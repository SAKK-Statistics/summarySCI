#' Summarize data as a table or a plot
#'
#' Wrapper around [summaryTable()], [boxplotSCI()] and [lineplotSCI()].
#'
#' @param type Which summary to produce: `"table"` or `"boxplot"` or `"lineplot"`.
#' @inheritDotParams summaryTable
#' @inheritDotParams boxplotSCI
# #' @inheritDotParams lineplotSCI does not work as not on Github yet
#' @export
summarizeSCI <- function(type = c("table", "boxplot", "lineplot"), ...) {
  type <- match.arg(type)
  switch(type,
         table = summaryTable(...),
         surv  = summarySurv(...)
  )
}
 #
