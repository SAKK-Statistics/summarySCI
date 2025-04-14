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
#' Options include "t.test", "oneway.test", "kruskal.test", "wilcox.test",
#' "paired.t.test", "paired.wilcox.test"
#' IF NULL (default), no p-value will be displayed.
#'
#' @param test_cat Test type for the p-value, for categorical variables.
#' IF NULL, no p-value will be displayed.
#'
#' @param ci_cont CI type for continuous variables. Options include ...
#' If NULL, no CI will be displayed.
#'
#' @param ci_cat CI type for categorical variables. Options include ...
#' If NULL, no CI will be displayed.
#'
#' @param conf_level Confidence level. Default to 0.95. Only used
#' if ci_cont and ci_cat are not NULL.
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
#' @param missing Indicates whether percentages for missings are shown (TRUE)
#' or not (FALSE). If "both", then both options are displayed.
#'
#' @param binary Only taken into account when missing = FALSE.
#' For binary variables, indicates whether all levels are
#' displayed (FALSE, default) or only one.
#'
#' @importFrom gtsummary tbl_summary
#' @import cardx dplyr Hmisc
#' @export


summaryTable <- function(data,
                         vars = NULL,
                         group = NULL,
                         stat_cont = "median_IQR" ,
                         stat_cat = NULL,
                         test_cont = NULL,
                         test_cat = NULL,
                         ci_cont = NULL,
                         ci_cat = NULL,
                         conf_level = 0.95,
                         layout_cont = NULL,
                         layout_cat = NULL,
                         digits_cont = 1,
                         digits_cat = 1,
                         missing = FALSE,
                         binary = FALSE){

  # --------- Some checks --------------------------------------------------- #

  if (is.null(test_cont) != is.null(test_cat)) {
    stop("Error: Both test_cont and test_cat should be either NULL or both should be non-NULL.")
  }

  if (is.null(ci_cont) != is.null(ci_cat)) {
    stop("Error: Both ci_cont and ci_cat should be either NULL or both should be non-NULL.")
  }

  # --------- A few required  functions ------------------------------------------

  geom_mean <- function(x, na.rm = TRUE) {
    exp(mean(log(x), na.rm = na.rm))
  }

  meanDiff <- function(data, variable, by, ...) {
    ttest_result <- t.test(data[[variable]] ~ as.factor(data[[by]]), ...)
    mean_diff <- ttest_result$estimate[1] - ttest_result$estimate[2]
    return(mean_diff)
  }


  meanDiffCI <- function(data, variable, by, ...) {
    ttest_result <- t.test(data[[variable]] ~ as.factor(data[[by]]), ...)
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

  # if vars = NULL, take all the variables (except group if not NULL).
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }

  # Summary stat for continuous variables
  stat_cont <- format_lookup[[stat_cont]]

  # binary variables as categorical or binary
  if(binary == FALSE){
    type_binary = "categorical"
  } else {
    type_binary = "dichotomous"
  }

  # -------------- # table for missing = FALSE (default in gtsummary)----- #
  if(missing == FALSE){

  tbl_noMissing <- tbl_summary(data = data,
                     include = vars,
                     by = group,
                     statistic = list(all_continuous() ~ stat_cont),
                     type = list(all_dichotomous() ~ type_binary),
                     missing_text = "(Missing)")


  if(!is.null(test_cont)) {
    tbl_noMissing <-  tbl_noMissing|>
      add_p(pvalue_fun = label_style_pvalue(digits = 2),
            test = list(all_categorical() ~ test_cat,
                        all_continuous() ~ test_cont))

  }


  if(!is.null(ci_cont)) {
    tbl_noMissing <- tbl_noMissing |>
      add_ci(method = list(all_continuous() ~ ci_cont,
                           all_categorical() ~ ci_cat),
             conf.level = conf_level)
  }

  # CMI: will work on it.
  #   if(stat_cont == "mean_sd" | stat_cont == "mean_se"){
  #     tbl_noMissing <- tbl_noMissing |>
  #       add_stat(fns = all_continuous() ~ meanDiff) |>
  #       add_stat(fns = all_continuous() ~ meanDiffCI) |>
  #       modify_header(add_stat_1 = "Mean difference") |>
  #       modify_header(add_stat_2 = "CI")
  #
  # }

# --------------------------  missing = FALSE -------------------------------- #
tbl <- tbl_noMissing
}
# --------------------------  missing = TRUE --------------------------------- #

if(missing != FALSE){

  data2 <- data

    for (i in colnames(data2|>
                       dplyr::select(vars, group))) {

      if (is.factor(data2[[i]]) == TRUE | is.character(data2[[i]])) {
        data2[[i]] <- forcats::fct_explicit_na(as.factor(data2[[i]]))
        if (!is.null(attr(data[[i]], "label"))) {
          Hmisc::label(data2[[i]]) <- attr(data[[i]], "label")
        }
      } else if (all(data2[[i]] %in% c(0, 1, NA))) {
        # data2[[i]]<- forcats::fct_explicit_na(factor(data2[[i]], labels=c("No", "Yes")))
        data2[[i]] <- forcats::fct_explicit_na(factor(data2[[i]]))
        if (!is.null(attr(data[[i]], "label"))) {
          Hmisc::label(data2[[i]]) <- attr(data[[i]], "label")
        }
      }
    }

    tbl_missing <- data2|>
      tbl_summary(by = group,
                  include = vars,
                  statistic = list(all_continuous() ~ stat_cont),
                  type = list(all_dichotomous() ~ "categorical"),
                  missing_text = "(Missing)")


    if(!is.null(ci_cont)) {
      tbl_missing <- tbl_missing |>
        add_ci(method = list(all_continuous() ~ ci_cont,
                             all_categorical() ~ ci_cat),
               conf.level = conf_level)
    }


      # tests displayed (!missings not counted in calculation!)
    # -> only take p-value from other table
    if (!is.null(test_cat)) {
      tbl_noMissing_short <- tbl_summary(data = data,
                                   include = vars,
                                   missing = "no",
                                   missing_text = "(Missing)",
                                   by = group,
                                   statistic = list(all_continuous() ~ stat_cont)
      ) |>
        add_p(pvalue_fun = label_style_pvalue(digits = 2),
              test = list(all_continuous() ~ test_cont,
                          all_categorical() ~ test_cat)) |>
        modify_column_hide(c("stat_1", "stat_2"))
    }

# merging table with missings and p-value
tbl_missingTRUE <- tbl_merge(tbls = list(tbl_missing, tbl_noMissing_short)) |>
        modify_spanning_header(everything()~NA_character_)

}

  if(missing == TRUE){
    tbl <- tbl_missingTRUE
  }



if(missing == "both"){
  tbl_noMissing2 <- tbl_summary(data = data,
                               include = vars,
                               by = group,
                               statistic = list(all_continuous() ~ stat_cont),
                               type = list(all_dichotomous() ~ "categorical"),
                               missing = "no")



  tbl_both <- tbl_merge(tbls = list(tbl_missing, tbl_noMissing2, tbl_noMissing_short)) |>
    modify_spanning_header(c("stat_1_1", "stat_2_1") ~ "**With missing**",
                           c("stat_1_2", "stat_2_2") ~ "**Without missing**",
                           c("p.value_3") ~ "")
  tbl <- tbl_both
}
tbl

}
