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
