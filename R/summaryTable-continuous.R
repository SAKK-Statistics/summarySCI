#' summaryTable for continuous data
#'
#' Produces a summary table for continuous data
#'
#' @param data A data frame or tibble containing the data to be plotted.
#'
#' @param vars Variables to include in the summary table. Default to
#' all variables present in the data.
#'
#' @param group A single column from data.
#' Summary statistics will be stratified according to this variable.
#' Default to NULL.
#'
#' @param stat_cont Summary statistic to display for continuous variables. Options
#' include "median_IQR" (default), "median_range", "mean_sd", "mean_se" and
#' "geomMean_sd".
#'
#' @param stat_cat Summary statistic to display for categorical variables.
#'
#' @param test_cont Test type for the p-value, for continuous variables.
#' Options include "t.test", "oneway.test", "kruskal.test", "wilcox.test" (default),
#' "paired.t.test", "paired.wilcox.test"
#' IF NULL, no p-value will be dispayed
#'
#' @param test_cat Test type for the p-value, for categorical variables.
#' IF NULL, no p-value will be dispayed
#'
#' @param ci_cont CI type for continuous variables. Options include ...
#' If NULL, no CI will be displayed.
#'
#' @param ci_cat CI type for categorical variables. Options include ...
#' If NULL, no CI will be displayed.
#'
#' @param conf_level Confidence level. Default to 0.95. Only used
#' if ci_cont or ci_cat is not NULL.
#'
#' @param layout_cont Layout for continuous variables. Is the CI in the same
#' column as the summary statistics. Only used if ci_cont is not NULL.
#' Default to ...
#'
#' @param layout_cat Layout for continuous variables. Is the CI in the same
#' column as the summary statistics. Only used if ci_cat is not NULL.
#' Default to ...
#'
#' @param digits_cont Digits for summary statistics and CI of continuous
#' variables
#'
#' @param digits_cat Digits for summary statistics and CI of categorical
#' variables
#'
#' @importFrom gtsummary tbl_summary
#' @import cardx
#' @export


summaryTable <- function(data,
                         vars = NULL, # if vars is null, all variables taken
                         group = NULL,
                         stat_cont = "median_IQR" ,
                         stat_cat = NULL,
                         test_cont = "wilcox.test",
                         test_cat = NULL,
                         ci_cont = "wilcox.test",
                         ci_cat = NULL,
                         conf_level = 0.95,
                         layout_cont = NULL,
                         layout_cat = NULL,
                         digits_cont = 1,
                         digits_cat = 1){

# --------- A few required  functions ------------------------------------------


geom_mean <- function(x, na.rm = TRUE) {
  exp(mean(log(x), na.rm = na.rm))
}

meanDiff <- function(data, variable, by, ...) {
    # Perform the t-test
    ttest_result <- t.test(data[[variable]] ~ as.factor(data[[by]]), ...)
    # Extract the mean difference
    mean_diff <- ttest_result$estimate[1] - ttest_result$estimate[2]
    return(mean_diff)
  }


meanDiffCI <- function(data, variable, by, ...) {
    # Perform the t-test
    ttest_result <- t.test(data[[variable]] ~ as.factor(data[[by]]), ...)
    # Extract the confidence interval for the mean difference
    ci_lower <- ttest_result$conf.int[1]
    ci_upper <- ttest_result$conf.int[2]
    return(paste0(round(ci_lower, 2), ", ", round(ci_upper, 2)))
}

se <- function(x) sd(x)/sqrt(length(x))

format_lookup <- list(
  mean_sd = "{mean} ({sd})",
  mean_se = "{mean} ({se})",
  median_range = "{median} ({min}, {max})",
  median_IQR = "{median} ({p25}, {p75})",
  geomMean_sd = "{geom_mean} ({sd})"
)
#  -----------------------------------------------------------------------------

# if vars = NULL, take all the variables.
if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }

# sumstat
cont <- format_lookup[[stat_cont]]


tbl <- tbl_summary(data = data,
                   include = vars,
                   by = group,
                   statistic = list(all_continuous() ~ cont),
)

if(!is.null(test_cont)) {
 tbl <-  tbl|>
  add_p(pvalue_fun = label_style_pvalue(digits = 2),
        test = all_continuous() ~ test_cont)

}


if(!is.null(ci_cont)) {
  tbl <- tbl |>
    add_ci(method = list(all_continuous() ~ ci_cont),
                         # all_categorical() ~ ci_cat
           conf.level = conf_level)

  if(stat_cont == "mean_sd" | stat_cont == "mean_se")
    tbl <- tbl |>
      add_stat(fns = all_continuous() ~ meanDiff) |>
      add_stat(fns = all_continuous() ~ meanDiffCI) |>
      modify_header(add_stat_1 = "Mean difference") |>
      modify_header(add_stat_2 = "CI")

}

tbl

}


# Example:

summaryTable(data = trial,
             group = "trt",
             stat_cont = "mean_sd")
library(gtsummary)
data(trial)
