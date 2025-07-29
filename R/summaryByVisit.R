#' Creates publication-ready summary tables grouped by visit
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


summaryByVisit<- function(data,
                          vars = NULL,
                          group = NULL,
                          stat_cont = "median_range",
                          stat_cat = "n_percent",
                          visitgroup = NULL,
                          visit = "visit"){

  # --------- Some checks --------------------------------------------------- #

  # Make sure that 'data' exists and that it is a data frame
  if (missing(data)) {
    stop("'data' must be specified.")
  }


  # --------- A few required  functions ------------------------------------------

  geom_mean <- function(x, na.rm = TRUE) {
    exp(mean(log(x), na.rm = na.rm))
  }

  meanDiff <- function(data, variable, by, ...) {
    ttest_result <- stats::t.test(data[[variable]] ~ as.factor(data[[by]]), ...)
    mean_diff <- ttest_result$estimate[1] - ttest_result$estimate[2]
    return(mean_diff)
  }


  meanDiffCI <- function(data, variable, by, ...) {
    ttest_result <- stats::t.test(data[[variable]] ~ as.factor(data[[by]]), ...)
    ci_lower <- ttest_result$conf.int[1]
    ci_upper <- ttest_result$conf.int[2]
    return(paste0(round(ci_lower, 2), ", ", round(ci_upper, 2)))
  }

  se <- function(x) stats::sd(x)/sqrt(length(x))

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
  # --------- categorical formats ------------------------------------------- #

  # Summary stat for continuous variables
  stat_cont <- format_lookup[[stat_cont]]
  stat_cat <- format_lookup_cat[[stat_cat]]

  # if vars = NULL, take all the variables (except group if not NULL).
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }

  data<-as.data.frame(data)

  tbl<-NULL
  tbx<-NULL

  myu<- unlist(as.vector(unique(data[visit])))

  # without visitgroup
  if (is.null(visitgroup)){
    for (i in 1:length(myu)){
      dat <- data[(data[visit]==paste0(myu[i])),]
      assign(paste0("t", i), dat|>
               dplyr::select(vars, group)|>
               tbl_summary(by=paste0(group),
                           missing="no",
                         #  type = vars~ "continuous",
                           label = vars ~ paste0(myu[i]))|>
               modify_header(update = list(label ~ paste0("**", visit ,"**")))|>
               add_n())

      if (i > 1){
        tbl <- tbl_stack(list(tbl, get(paste0("t", i))), quiet = TRUE)
      }
      else{
        tbl<-t1
      }
    }
  }

  # with visitgroup
  myv<- unlist(as.vector((data|>select(visit, visitgroup)|> dplyr::distinct())[2]))

  if (!is.null(visitgroup)){
    tbl<-c("")
    for (i in 1:length(myu)){
      dat <- data[(data[visit]==paste0(myu[i])),]
      assign(paste0("t", i), dat|>
               dplyr::select(vars, group, visitgroup)|>
               tbl_summary(by=paste0(group),
                           missing="no",
                           statistic = list(all_continuous() ~ stat_cont),
                           label = vars ~ c(paste0(myu[i])),
                           type=all_continuous() ~ "continuous")|>
               modify_header(update = list(label ~ paste0("**", visit,"**")))|>
               add_n()|>
               modify_table_body(
                 ~ .x %>%
                   dplyr::mutate(vg_ = dat[visitgroup]))
      )
      if (i > 1){
        tbl <- tbl_stack(list(tbl, get(paste0("t", i))), quiet = TRUE)
      }
      else{
        tbl<-t1
      }
    }
  }
  tbl
}
