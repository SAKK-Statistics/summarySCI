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
#' @param label A list containing the labels that should be used for the
#' variables in the table. If NULL, labels are automaticall taken from the
#' dataset. If no label present, the variable name is taken.
#'
#' @param levels = A vector containing the values indicating presence of
#' the factor level. Included by defaul are "1", "yes", "Yes".
#'
#' @param stat_cat Summary statistic to display for categorical variables.
#' Options include "n_percent" (default) and "n", and "n_N".
#'
#' @param test Logical. Indicates whether p-values are displayed (TRUE)
#' or not (FALSE). Default to FALSE
#'
#' @param test_cat Test type used to calculated the p-value
#' for categorical variables.  Only used if `test = TRUE`.
#' Options include "chisq.test", "chisq.test.no.correct", "fisher.test" (default).
#'
#' @param ci Logical. Indicates whether CI are displayed (TRUE) or
#' not (FALSE). Default to FALSE.
#'
#' @param ci_cont Confidence interval method for continuous variables.
#'  Only used if `ci = TRUE`.
#' Options include "t.test" (default) and "wilcox.test".
#'
#' @param ci_cat Confidence interval method for categorical variables.
#' Options include "wilson" (default), "wilson.no.correct", "clopper.pearson",
#' "wald", "wald.no.correct", "agresti.coull" and "jeffreys".
#' If NULL, no CI will be displayed.
#'
#' @param conf_level Confidence level. Default to 0.95.
#'
#' @param overall Logical. If TRUE, an additional column with the total is
#' added to the table. Default to FALSE.
#'
#' @import cardx dplyr gtsummary forcats
#' @importFrom Hmisc label
#' @importFrom stats sd t.test
#' @export


summaryLevels <- function(data,
                         vars = NULL,
                         group = NULL,
                         label = NULL,
                         levels = NULL,
                         stat_cat = "n_percent",
                         test = FALSE,
                         test_cat = "fisher.test",
                         ci = FALSE,
                         ci_cat = "wilson",
                         conf_level = 0.95,
                         overall = FALSE){

  # --------- Some checks --------------------------------------------------- #

  # Make sure that 'data' exists and that it is a data frame
  if (missing(data)) {
    stop("'data' must be specified.")
  }

  # Stop if vars are not categorical
  is_not_categorical <- function(df) {
    any(!sapply(df, function(x) is.factor(x) || is.character(x)))
  }
  if (is_not_categorical(data[vars])){
    stop("'vars' must be categorical.")
  }

  data <- data.frame(data)

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

  # --------- categorical formats ------------------------------------------- #
  format_lookup_cat <-
    list(
      n_percent = "{n} ({p}%)",
      n = "{n}",
      n_N = "{n}/{N}"
    )
  stat_cat <- format_lookup_cat[[stat_cat]]

  # if vars = NULL, take all the variables (except group if not NULL).
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }

  # if levels need to be recoded -------------------------------------------- #
  if (!is.null(levels)){
  for (i in 1:length(vars)){
    data[vars[i]]<-as.numeric(ifelse(data[vars[i]]==levels, 1, 0))
    }
  }

  # create tables for different combinations -------------------------------- #
  tbl<-NULL

      for (i in 1:length(vars)){
        if (is.null(group)){
          assign(paste0("t", i), data|>
                   dplyr::select(vars[i])|>
                   tbl_summary(missing="no"))
        }
        if (!is.null(group)){
          if (overall==FALSE & test==FALSE){
            assign(paste0("t", i), data|>
                 dplyr::select(vars[i], group)|>
                 tbl_summary(by= paste0(group), missing="no"))
          }
          if (overall==TRUE & test==FALSE){
            assign(paste0("t", i), data|>
                     dplyr::select(vars[i], group)|>
                     tbl_summary(by= paste0(group), missing="no")|>
                     add_overall())
          }
          if (overall==TRUE & test==TRUE){
            assign(paste0("t", i), data|>
                     dplyr::select(vars[i], group)|>
                     tbl_summary(by= paste0(group), missing="no")|>
                     add_overall()|>
                     add_p(pvalue_fun = label_style_pvalue(digits = 2),
                           test = list((all_categorical() ~ test_cat))))
          }
          if (overall==FALSE & test==TRUE){
            assign(paste0("t", i), data|>
                     dplyr::select(vars[i], group)|>
                     tbl_summary(by= paste0(group), missing="no")|>
                     add_p(pvalue_fun = label_style_pvalue(digits = 2),
                           test = list((all_categorical() ~ test_cat))))
          }
        }
      if (i > 1){
        if (ci==FALSE){
          tbl <- tbl_stack(list(tbl, get(paste0("t", i))))
        }
        if (ci==TRUE){
         assign(paste0("x", i), get(paste0("t", i))|>
            add_ci(method = list(all_categorical() ~ ci_cat_gt),
                   conf.level = conf_level,
                   statistic = list(all_categorical() ~ "[{conf.low}%, {conf.high}%]")))
          tbl <- tbl_stack(list(tbl, get(paste0("x", i))))
        }
      }
        else {
          if (ci==FALSE){
            tbl <- t1
          }
          if (ci==TRUE){
            tbl <- t1|>
              add_ci(method = list(all_categorical() ~ ci_cat_gt),
                     conf.level = conf_level,
                     statistic = list(all_categorical() ~ "[{conf.low}%, {conf.high}%]"))
          }
        }
      }
  # ---------------------------- add label if any --------------------------- #
  if (!is.null(label)){
    tbl|>
      modify_header(update = list(label ~ paste0("**", label, "**")))|>
      modify_table_styling(
        columns = label,
        footnote = "More than one entry possible"
      )
  }
  else{
    tbl|>
      modify_table_styling(
        columns = label,
        footnote = "More than one entry possible"
      )
  }
}
