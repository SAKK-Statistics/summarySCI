#' summaryTable for continuous data
#'
#' Produces a summary table for continuous data
#'
#'
#' @param by A single column from data.
#' Summary statistics will be stratified by this variable. Default is NULL.
#' (from tbl_summary)
#'
#' @param continuous Summary statistic to display for continuous variables. Options
#' include "median_IQ" (default), "median_range", "mean_sd", "mean_se" and
#' "geomMean_sd".
#'
#' @param test_continuous Test for continuous variables. Options
#' include "t.test" (default), "oneway.test", "kruskal.test", "wilcox.test",
#' "paired.t.test", "paired.wilcox.test"
#'
#' @param ci Confidence interval
#' @param conf.level Confidence level. Default to 0.95
#'
#'
#' @importFrom gtsummary tbl_summary
#' @import cardx
#' @export


summaryTable <- function(data,
                         by = NULL,
                         continuous = "median_IQ",
                        test_continuous = "t.test",
                        ci = "yes",
                        conf.level = 0.95
                           ){

geom_mean <- function(x, na.rm = TRUE) {
      exp(mean(log(x), na.rm = na.rm))
}

se <- function(x) sd(x)/sqrt(length(x))

if(continuous == "mean_sd"){
  cont <- "{mean} ({sd})"
}

if(continuous == "mean_se"){
  cont <- "{mean} ({se})"
}

if(continuous == "median_range"){
  cont <- "{median} ({min}, {max})"
}

if(continuous == "median_IQ") {
  cont <- "{median} ({p25}, {p75})"
}

if(continuous == "geomMean_sd"){
  cont <- "{geom_mean} ({sd})"
}


tbl <- tbl_summary(data = data, by = by,
            statistic = list(
              all_continuous() ~ cont)
)

if(!is.null(by)) {
 tbl <-  tbl|>
  add_p(pvalue_fun = label_style_pvalue(digits = 2),
        test = all_continuous() ~ test_continuous)

}

if(!is.null(ci)) {
  tbl <- tbl |>
    add_ci(conf.level = conf.level)
    # add_ci(method = list(all_continuous() ~ "t.test"))
}
tbl
}
