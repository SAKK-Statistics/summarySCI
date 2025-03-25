#' summaryTable for continuous data
#'
#' Produces a summary table for continuous data
#'
#' @param continuous Summary statistic to display for continuous variables. Options
#' include "median_IQ" (default), "median_range", "mean_sd".
#'
#' @param by A single column from data.
#' Summary statistics will be stratified by this variable. Default is NULL.
#' (from tbl_summary)
#'
#'
#'
#' @importFrom gtsummary tbl_summary
#' @import cardx
#' @export


summaryTable <- function(data, by = NULL, continuous = "median_IQ"
                         # test_continuous =
                           ){

if(continuous == "mean_sd"){
  cont <- "{mean} ({sd})"
}

if(continuous == "median_range"){
  cont <- "{median} ({min}, {max})"
}

if(continuous == "median_IQ") {
  cont <- "{median} ({p25}, {p75})"
}

  # ... to be continued

tbl <- tbl_summary(data = data, by = by,
            statistic = list(
              all_continuous() ~ cont)
)
if(!is.null(by)) {
  tbl|>
  add_p(pvalue_fun = label_style_pvalue(digits = 2)) # I am here
}


}
