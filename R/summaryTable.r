#' Creates publication-ready summary tables
#'
#' Creates publication-ready summary tables based on the gtsummary
#' package. test
#'
#' @param data A data frame or tibble containing the data to be summarized.
#'
#' @param vars Variables to include in the summary table. Default to
#' all variables present in the data except `group`.
#'
#' @param group A single column from `data`.
#' Summary statistics will be stratified according to this variable.
#' Default to NULL.
#'
#' @param stat_cont Summary statistic to display for continuous variables. Options
#' include "median_IQR", "median_range" (default), "mean_sd", "mean_se" and
#' "geomMean_sd".
#'
#' @param stat_cat Summary statistic to display for categorical variables.
#' Options include "n_percent" (default) and "n", and "n_N".
#'
#' @param test_cont Test type used to calculate the p-value
#' for continuous variables. Only used if `group` is not NULL.
#' Options include "t.test", "oneway.test", "kruskal.test", "wilcox.test",
#' "paired.t.test", "paired.wilcox.test"
#' IF NULL (default), no p-value will be displayed.
#'
#' @param test_cat Test type used to calculated the p-value
#' for categorical variables. Only used if `group` is not NULL.
#' Options include "chisq.test", "chisq.test.no.correct", "fisher.test".
#' IF NULL, no p-value will be displayed.
#'
#' @param ci_cont Confidence interval method for continuous variables.
#' Options include "t.test" and "wilcox.text".
#' If NULL, no CI will be displayed.
#'
#' @param ci_cat Confidence interval method for categorical variables.
#' Options include "wilson", "wilson.no.correct", "clopper.pearson",
#' "wald", "wald.no.correct", "agresti.coull" and "jeffreys".
#' If NULL, no CI will be displayed.
#'
#' @param conf_level Confidence level. Default to 0.95.
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
#' variables. Default to 1.
#'
#' @param digits_cat Digits for summary statistics and CI of categorical
#' variables. Default to 0.
#'
#' @param missing Indicates whether percentages for missings are shown (TRUE, default)
#' or not (FALSE). If "both", then both options are displayed next to each other.
#'
#' @param missing_text String indicating text shown on missing row. Default to
#' "Missing".
#'
#' @param binary Only taken into account when missing = FALSE.
#' For binary variables, indicates whether all levels are
#' displayed (FALSE, default) or only one.
#'
#' @param overall Logical. If TRUE, an additional column with the total is
#' added to the table. Default to FALSE.
#'
#' @import cardx dplyr Hmisc gtsummary
#' @export


summaryTable <- function(data,
                         vars = NULL,
                         group = NULL,
                         stat_cont = "median_range",
                         stat_cat = "n_percent",
                         test_cont = NULL,
                         test_cat = NULL,
                         ci_cont = NULL,
                         ci_cat = NULL,
                         conf_level = 0.95,
                         layout_cont = NULL,
                         layout_cat = NULL,
                         digits_cont = 1,
                         digits_cat = 0,
                         missing = TRUE,
                         binary = FALSE,
                         missing_text = "Missing",
                         overall = FALSE){

  # --------- Some checks --------------------------------------------------- #

  # Make sure that 'data' exists and that it is a data frame
  if (missing(data)) {
    stop("'data' must be specified.")
  }
  data <- data.frame(data)

  # if (is.null(test_cont) != is.null(test_cat)) {
  #   stop("Error: Both 'test_cont' and 'test_cat' should be either NULL or both should be non-NULL.")
  # }

  # if (is.null(ci_cont) != is.null(ci_cat)) {
  #   stop("Error: Both 'ci_cont' and 'ci_cat' should be either NULL or both should be non-NULL.")
  # }

  if((is.null(group) & !is.null(test_cont)) | (is.null(group) & !is.null(test_cat))){
    stop("Error: 'group' needs to be given for a test to be calculated.")
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

  format_lookup_cat <-
    list(
      n_percent = "{n} ({p}%)",
      n = "{n}",
      n_N = "{n}/{N}"
    )
  #  -----------------------------------------------------------------------------

  # if vars = NULL, take all the variables (except group if not NULL).
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }

  # Summary stat for continuous variables
  stat_cont <- format_lookup[[stat_cont]]
  stat_cat <- format_lookup_cat[[stat_cat]]

  # binary variables as categorical or binary
  if(binary == FALSE){
    type_binary = "categorical"
  } else {
    type_binary = "dichotomous"
  }

  if(!is.null(ci_cat)){

  if(ci_cat == "clopper-pearson"){
    ci_cat_gt <- "exact"
  } else if(ci_cat == "wilson" |
            ci_cat == "wilson.no.correct"|
            ci_cat == "clopper.pearson" |
            ci_cat == "wald"|
            ci_cat == "wald.no.correct" |
            ci_cat == "agresti.coull"|
            ci_cat == "jeffreys") {
    ci_cat_gt <- ci_cat
  }else{
    stop(paste0("The chosen CI method '", ci_cat, "' does not exist or is not yet implemented."))
    }
  }


  # -------------- # table for missing = FALSE (default in gtsummary)----- #
  if(missing == FALSE){

  tbl_noMissing <- tbl_summary(data = data,
                     include = vars,
                     by = group,
                     statistic = list(all_continuous() ~ stat_cont,
                                      all_categorical() ~ stat_cat),
                     type = list(all_dichotomous() ~ type_binary),
                     missing_text = missing_text,
                     digits = list(all_categorical() ~ digits_cat,
                                   all_continuous() ~ digits_cont))


  if(!is.null(test_cont)) {
    tbl_noMissing <-  tbl_noMissing|>
      add_p(pvalue_fun = label_style_pvalue(digits = 2),
            test = list(all_continuous() ~ test_cont))

  }


  if(!is.null(test_cat)) {
    tbl_noMissing <-  tbl_noMissing|>
      add_p(pvalue_fun = label_style_pvalue(digits = 2),
            test = list(all_categorical() ~ test_cat))

  }


  if(!is.null(ci_cont)) {
    tbl_noMissing <- tbl_noMissing |>
      add_ci(method = list(all_continuous() ~ ci_cont),
             conf.level = conf_level,
             statistic = list(all_continuous() ~ "[{conf.low}, {conf.high}]"))
  }


  if(!is.null(ci_cat)) {
    tbl_noMissing <- tbl_noMissing |>
      add_ci(method = list(all_categorical() ~ ci_cat_gt),
             conf.level = conf_level,
             statistic = list(all_categorical() ~ "[{conf.low}%, {conf.high}%]"))
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

if(overall == TRUE & !is.null(group)){
  tbl <- tbl %>%
    add_overall(last = TRUE)
}
}
# --------------------------  missing = TRUE --------------------------------- #

if(missing != FALSE){

  data2 <- data

    for (i in colnames(data2|>
                       dplyr::select(vars, group))) {

      if (is.factor(data2[[i]]) == TRUE | is.character(data2[[i]])) {
        data2[[i]] <- forcats::fct_explicit_na(as.factor(data2[[i]]), na_level = missing_text)
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
                  statistic = list(all_continuous() ~ stat_cont,
                                   all_categorical() ~ stat_cat),
                  type = list(all_dichotomous() ~ "categorical"),
                  missing_text = missing_text,
                  digits = list(all_categorical() ~ digits_cat,
                                all_continuous() ~ digits_cont))


    if(!is.null(ci_cont)) {
      tbl_missing <- tbl_missing |>
        add_ci(method = list(all_continuous() ~ ci_cont),
               conf.level = conf_level,
               statistic = list(all_continuous() ~ "[{conf.low}, {conf.high}]"))
    }


    if(!is.null(ci_cat)) {
      tbl_missing <- tbl_missing |>
        add_ci(method = list(all_categorical() ~ ci_cat_gt),
               conf.level = conf_level,
               statistic = list(all_categorical() ~ "[{conf.low}%, {conf.high}%]"))
    }


      # tests displayed (!missings not counted in calculation!)
    # -> only take p-value from other table
    if (!is.null(test_cat)) {
      tbl_noMissing_short <- tbl_summary(data = data,
                                   include = vars,
                                   missing = "no",
                                   missing_text = missing_text,
                                   by = group,
                                   statistic = list(all_categorical() ~ stat_cat),
                                   digits = list(all_categorical() ~ digits_cat)
      ) |>
        add_p(pvalue_fun = label_style_pvalue(digits = 2),
              test = list(all_categorical() ~ test_cat)) |>
        modify_column_hide(c("stat_1", "stat_2"))

      if(overall == TRUE & !is.null(group)){
          tbl_noMissing_short <- tbl_noMissing_short %>%
            add_overall(last = TRUE)
        }
    }

    if (!is.null(test_cont)) {
      tbl_noMissing_short <- tbl_summary(data = data,
                                         include = vars,
                                         missing = "no",
                                         missing_text = missing_text,
                                         by = group,
                                         statistic = list(all_continuous() ~ stat_cont),
                                         digits = list(all_continuous() ~ digits_cont)
      ) |>
        add_p(pvalue_fun = label_style_pvalue(digits = 2),
              test = list(all_continuous() ~ test_cont)) |>
        modify_column_hide(c("stat_1", "stat_2"))

      if(overall == TRUE & !is.null(group)){
        tbl_noMissing_short <- tbl_noMissing_short %>%
        add_overall(last = TRUE)
      }
    }

# merging table with missings and p-value
    if(!is.null(test_cont)| !is.null(test_cat)){
tbl_missingTRUE <- tbl_merge(tbls = list(tbl_missing, tbl_noMissing_short)) |>
        modify_spanning_header(everything()~NA_character_)

    } else {
  tbl_missingTRUE <- tbl_missing

  if(overall == TRUE & !is.null(group)){
    tbl_missingTRUE <- tbl_missingTRUE %>%
      add_overall(last = TRUE)
  }
}

}

  if(missing == TRUE){
    tbl <- tbl_missingTRUE
  }



if(missing == "both"){
  tbl_noMissing2 <- tbl_summary(data = data,
                               include = vars,
                               by = group,
                               statistic = list(all_continuous() ~ stat_cont,
                                                all_categorical() ~ stat_cat),
                               type = list(all_dichotomous() ~ "categorical"),
                               missing = "no",
                               missing_text = missing_text,
                               digits = list(all_categorical() ~ digits_cat,
                                             all_continuous() ~ digits_cont))


if(!is.null(test_cont) | !is.null(test_cat)){
  tbl_both <- tbl_merge(tbls = list(tbl_missing, tbl_noMissing2, tbl_noMissing_short)) |>
    modify_spanning_header(c("stat_1_1", "stat_2_1") ~ "**With missing**",
                           c("stat_1_2", "stat_2_2") ~ "**Without missing**",
                           c("p.value_3") ~ "")
}

  if(is.null(test_cont) & is.null(test_cat)){
    tbl_both <- tbl_merge(tbls = list(tbl_missing, tbl_noMissing2))|>
      modify_spanning_header(c("stat_0_1") ~ "**With missing**",
                             c("stat_0_2") ~ "**Without missing**")
  }
  tbl <- tbl_both
}
tbl
}
