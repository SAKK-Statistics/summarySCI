#' summaryTable for continuous data
#'
#' Produces a summary table for continuous data
#'
#'
#'
#'
#'
#' @import
#' @export


summaryTable <- function(data, continuous = "mean_sd"){

if(continuous == "mean_sd"){
  cont <- "{mean} ({sd})"
}

  # ... to be continued

tbl_summary(data = data,
            statistic = list(
              all_continuous() ~ cont)
)


}
