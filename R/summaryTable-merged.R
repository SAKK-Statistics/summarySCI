#' Creates publication-ready summary tables
#'
#' Creates publication-ready summary tables based on the gtsummary
#' package.
#'
#' @param data A data frame or tibble containing the data to be plotted.
#'
#' @param vars Variables to include in the summary table. Default to
#' all variables present in the data if `group` is NULL,
#' otherwise to all variables except `group`.
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
#' @param missing Indicate whether percentages for missings are shown (TRUE)
#' or not (FALSE). If "both", then both options are displayed.
#'
#' @importFrom gtsummary tbl_summary
#' @import cardx dplyr Hmisc
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
                         digits_cat = 1,
                         missing = FALSE){

  # --------- A few required  functions ------------------------------------------


  # Comment CMI: Maybe enough to have these packaged in the "Imports" field?
  # check if required packages are install and do so if not
  list.of.packages <- c("gtsummary", "dplyr", "Hmisc")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) {install.packages(new.packages)}

  # Comment CMI: using :: instead of library()
  #load packages
  # library(gtsummary)
  # library(dplyr)
  # library(Hmisc)


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

# --------------------------  missing = FALSE -------------------------------- #

  tbl_noMissing <- tbl_summary(data = data,
                     include = vars,
                     by = group,
                     statistic = list(all_continuous() ~ cont)
  )

  if(!is.null(test_cat)) {
    tbl_noMissing <-  tbl_noMissing|>
      add_p(pvalue_fun = label_style_pvalue(digits = 2),
            test = list(
              # all_continuous() ~ test_cont,
                        all_categorical() ~ test_cat))

  }

  if(!is.null(test_cont)) {
    tbl_noMissing <-  tbl_noMissing|>
      add_p(pvalue_fun = label_style_pvalue(digits = 2),
            test = list(
               all_continuous() ~ test_cont))

  }


  if(!is.null(ci_cont)) {
    tbl_noMissing <- tbl_noMissing |>
      add_ci(method = list(all_continuous() ~ ci_cont,
                           all_categorical() ~ ci_cat),
             conf.level = conf_level)
  }

  #   if(stat_cont == "mean_sd" | stat_cont == "mean_se"){
  #     tbl_noMissing <- tbl_noMissing |>
  #       add_stat(fns = all_continuous() ~ meanDiff) |>
  #       add_stat(fns = all_continuous() ~ meanDiffCI) |>
  #       modify_header(add_stat_1 = "Mean difference") |>
  #       modify_header(add_stat_2 = "CI")
  #
  # }

if(missing == FALSE){
tbl <- tbl_noMissing
}
# --------------------------  missing = TRUE -------------------------------- #

  data2 <- data

    for (i in colnames(data2|>
                       dplyr::select(vars, group))) {
      if (is.factor(data2[[i]]) == TRUE | is.character(data2[[i]])) {
        data2[[i]]<- forcats::fct_explicit_na(as.factor(data2[[i]]))
        if (!is.null(attr(data[[i]], "label"))) {
          Hmisc::label(data2[[i]]) <- attr(data[[i]], "label")
        }
      } else if (all(data2[[i]] %in% c(0, 1, NA))) {
        data2[[i]]<- forcats::fct_explicit_na(factor(data2[[i]], labels=c("No", "Yes")))
        if (!is.null(attr(data[[i]], "label"))) {
          Hmisc::label(data2[[i]]) <- attr(data[[i]], "label")
        }
      }
}
      tbl_missing <- data2|>
        tbl_summary(by = group,
                    include = vars,
                    type = list(all_dichotomous() ~ "categorical"))
      # tests displayed
    if (!is.null(test_cat)) {
      tbl_noMissing_short <- tbl_summary(data = data,
                                   include = vars,
                                   missing = "no",
                                   by = group,
                                   statistic = list(all_continuous() ~ cont)
      ) |>
        add_p(pvalue_fun = label_style_pvalue(digits = 2),
              test = list(all_continuous() ~ test_cont,
                          all_categorical() ~ test_cat)) |>
        modify_column_hide(c("stat_1", "stat_2"))
    }

tbl_missingTRUE <- tbl_merge(tbls = list(tbl_missing, tbl_noMissing_short)) |>
        modify_spanning_header(everything()~NA_character_)

if(missing == TRUE){
      tbl <- tbl_missingTRUE
}

  # --------------------------  missing = both -------------------------------- #


      # for (i in colnames(data3|>
      #                    dplyr::select(vars, group))) {
      #   if (all(data3[[i]] %in% c(0, 1, NA))) {
      #     data3[[i]]<- factor(data3[[i]], labels=c("No", "Yes"))
      #     if (!is.null(attr(data[[i]], "label"))) {
      #       Hmisc::label(data3[[i]]) <- attr(data[[i]], "label")
      #     }
      #   }
      # }
      # t0<-data3|>
      #   dplyr::select(vars, group)|>
      #   tbl_summary(by=group, missing="no",
      #               type = list(all_dichotomous() ~ "categorical"))|>
      #   add_p(test=list(all_categorical() ~  test_cat))
      #
      #
      # t<- tbl_merge(tbls = list(t00, t0))|>
      #   modify_spanning_header(c("stat_1_1", "stat_2_1") ~ "**With missing**",
                               # c("stat_1_2", "stat_2_2", "p.value_2") ~ "**Without missing**")
# Work in progress
  tbl_both <- tbl_merge(tbls = list( tbl_missing, tbl_noMissing)) |>
    modify_spanning_header(c("stat_1_1", "stat_2_1") ~ "**With missing**",
                           c("stat_1_2", "stat_2_2") ~ "**Without missing**")
if(missing == "both"){
  tbl <- tbl_both
}
tbl

}
