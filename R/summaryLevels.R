#' summaryLevels
#'
#' Collapses factor levels from multiple columns into one and creates summary table.
#'
#' @param data A data frame or tibble containing the data to be summarized.
#'
#' @param vars Variables to include in the summary table.
#' Need to be specified with quotes, e.g. `"score"` or `c("score", "age_cat")`.
#' Default to
#' all variables present in the data except `group`.
#'
#' @param group A single column from `data`.
#' Need to be specified with quotes, e.g. `"treatment"`.
#' Summary statistics will be stratified according to this variable.
#' Default to NULL.
#'
#' @param labels A list containing the labels that should be used for the
#' variables in the table. If NULL, labels are automatically taken from the
#' dataset. If no label present, the variable name is taken.
#'
#' @param levels = A vector containing the values indicating presence of
#' the factor level. Included by default are "1", "yes", "Yes".
#'
#' @param stat_cat Summary statistic to display for categorical variables.
#' Options include "n_percent" (default) and "n", and "n_N".
#'
#' @param test Logical. Indicates whether p-values are displayed (TRUE)
#' or not (FALSE). Default to FALSE
#'
#' @param test_cat Test type used to calculated the p-value
#' for categorical variables.  Only used if `test = TRUE`.
#' Options include "fisher.test" (default), "chisq.test", "chisq.test.no.correct".
#' If NULL, the function decides itself: "chisq.test.no.correct" for categorical
#' variables with all expected
#' cell counts >=5, and "fisher.test" for categorical variables with
#' any expected cell count <5.
#'
#' @param ci Logical. Indicates whether CI are displayed (TRUE) or
#' not (FALSE). Default to FALSE.
#'
#' @param ci_cat Confidence interval method for categorical variables.
#' Options include "wilson" (default), "wilson.no.correct", "clopper.pearson",
#' "wald", "wald.no.correct", "agresti.coull" and "jeffreys".
#' If NULL, no CI will be displayed.
#'
#' @param conf_level Numeric. Confidence level. Default to 0.95.
#'
#' @param digits_cat Numeric. Digits for summary statistics and CI of categorical
#' variables. Default to 0.
#'
#' @param as_flex_table Logical. If TRUE (default) the gtsummary object is
#' converted to a flextable object. Useful when rendering to Word.
#'
#' @param overall Logical. If TRUE, an additional column with the total is
#' added to the table. Default to FALSE.
#'
#' @param as_flex_table Logical. If TRUE (default) the gtsummary object is
#' converted to a flextable object. Useful when rendering to Word.
#'
#' @param border Logical. If TRUE, a border will be drawn around the table. Only
#' available if flex_table = TRUE. Default is TRUE.
#'
#' @param word_output Logical. If TRUE, the table is also saved in a word document.
#'
#' @param file_name Character string.
#' Specify the name of the Word document containing the table.
#' Only used when `word_output` is TRUE. Needs to end with ".docx".
#'
#' @import cardx dplyr gtsummary forcats
#' @importFrom Hmisc label
#' @importFrom stats sd t.test na.omit
#' @importFrom flextable autofit width flextable_dim
#' @importFrom officer read_docx
#' @export



summaryLevels <- function(data,
                         vars = NULL,
                         group = NULL,
                         labels = NULL,
                         levels = NULL,
                         stat_cat = "n_percent",
                         test = FALSE,
                         test_cat = "fisher.test",
                         ci = FALSE,
                         ci_cat = "wilson",
                         conf_level = 0.95,
                         digits_cat = 0,
                         overall = FALSE,
                         as_flex_table = TRUE,
                         border = TRUE,
                         word_output = FALSE,
                         file_name = paste0("SummaryLevels_", format(Sys.Date(), "%Y%m%d"), ".docx")){

  # --------- Some checks --------------------------------------------------- #

  label <- labels


  # Make sure that 'data' exists and that it is a data frame
  if (missing(data)) {
    stop("'data' must be specified.")
  }

  # make sur data is a data frame
  data <- as.data.frame(data)

  # NA should be ignored (treated as absent). Thus, replace any NA with 0
  data[is.na(data)] <- 0

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
                   gtsummary::tbl_summary(missing="no",
                                          digits = list(gtsummary::all_categorical() ~ digits_cat)))
        }
        if (!is.null(group)){
          if (overall==FALSE & test==FALSE){
            assign(paste0("t", i), data|>
                 dplyr::select(vars[i], group)|>
                 gtsummary::tbl_summary(by= paste0(group), missing="no",
                                        digits = list(gtsummary::all_categorical() ~ digits_cat)))
          }
          if (overall==TRUE & test==FALSE){
            assign(paste0("t", i), data|>
                     dplyr::select(vars[i], group)|>
                     gtsummary::tbl_summary(by= paste0(group), missing="no",
                                            digits = list(gtsummary::all_categorical() ~ digits_cat))|>
                     gtsummary::add_overall())
          }
          if (overall==TRUE & test==TRUE){
            assign(paste0("t", i), data|>
                     dplyr::select(vars[i], group)|>
                     gtsummary::tbl_summary(by= paste0(group), missing="no",
                                            digits = list(gtsummary::all_categorical() ~ digits_cat))|>
                     gtsummary::add_overall()|>
                     gtsummary::add_p(pvalue_fun = gtsummary::label_style_pvalue(digits = 2),
                           test = list((gtsummary::all_categorical() ~ test_cat))))
          }
          if (overall==FALSE & test==TRUE){
            assign(paste0("t", i), data|>
                     dplyr::select(vars[i], group)|>
                     gtsummary::tbl_summary(by= paste0(group), missing="no",
                                            digits = list(gtsummary::all_categorical() ~ digits_cat))|>
                     gtsummary::add_p(pvalue_fun = gtsummary::label_style_pvalue(digits = 2),
                           test = list((gtsummary::all_categorical() ~ test_cat))))
          }
        }
      if (i > 1){
        if (ci==FALSE){
          tbl <- gtsummary::tbl_stack(list(tbl, get(paste0("t", i))))
        }
        if (ci==TRUE){
         assign(paste0("x", i), get(paste0("t", i))|>
            gtsummary::add_ci(method = list(gtsummary::all_categorical() ~ ci_cat_gt),
                   conf.level = conf_level,
                   statistic = list(gtsummary::all_categorical() ~ "[{conf.low}%, {conf.high}%]")))
          tbl <- gtsummary::tbl_stack(list(tbl, get(paste0("x", i))))
        }
      }
        else {
          if (ci==FALSE){
            tbl <- t1
          }
          if (ci==TRUE){
            tbl <- t1|>
              gtsummary::add_ci(method = list(gtsummary::all_categorical() ~ ci_cat_gt),
                     conf.level = conf_level,
                     statistic = list(gtsummary::all_categorical() ~ "[{conf.low}%, {conf.high}%]"))
          }
        }
      }
  # ---------------------------- add label if any --------------------------- #
  if (!is.null(label)){
    tbl<-tbl|>
      gtsummary::modify_header(update = list(label ~ paste0("**", label, "**")))|>
      gtsummary::modify_table_styling(
        columns = label,
        footnote = "More than one entry possible"
      )
  }
  else{
    tbl<-tbl|>
      gtsummary::modify_table_styling(
        columns = label,
        footnote = "More than one entry possible"
      )
  }
  if(as_flex_table == TRUE | word_output == TRUE){
    if (border == TRUE){
      tbl_print <- FitFlextableToPage(gtsummary::as_flex_table(tbl)|>
                                        flextable::border_outer(part = "header")|>
                                        flextable::border_outer(part = "body") )
    } else {
      tbl_print <- FitFlextableToPage(gtsummary::as_flex_table(tbl))
    }
  } else {
    tbl_print <- tbl
  }


  if (word_output == TRUE) {

    # Create Word document
    doc <- officer::read_docx()
    doc <- flextable::body_add_flextable(doc, value = tbl_print)

    # Save to specified location
    print(doc, target = file_name)

    message("Table saved to: ", normalizePath(file_name))
  }

  tbl_print

}
