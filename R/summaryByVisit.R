#' Creates publication-ready summary tables for continuous data grouped, by visit
#'
#' @param data A data frame or tibble containing the data to be summarized.
#'
#' @param vars Continuous variables to include in the summary table.
#' Need to be specified with quotes, e.g. `"age"` or `c("age", "response")`. Default to
#' all variables present in the data except `group`.
#'
#' @param group A single column from `data`.
#' Need to be specified with quotes, e.g. `"treatment"`.
#' Summary statistics will be stratified according to this variable.
#' Default to NULL. A maximum of 3 groups are currently supported.
#'
#' @param labels A list containing the labels that should be used for the
#' variables in the table. If NULL, labels are automatically taken from the
#' dataset. If no label present, the variable name is taken.
#'
#' @param stat_cont Summary statistic to display for continuous variables.
#' Options include "median_IQR", "median_range" (default), "mean_sd",
#' "mean_se" and "geomMean_sd".
#'
#' @param stat_cat Summary statistic to display for categorical variables.
#' Options include "n", "n_N" and "n_percent" (default).
#'
#' @param visit Name of the stratum for which summary statistics are
#' displayed by line. Typically, this would be `"visit"`.
#'
#' @param order A numerical variable defining the visit order.
#'
#' @param visitgroup A grouping variable for the stratum for which summary
#' statistics are displayed by line. Must be an ordered factor.
#' Typically, this would be a visit group such as e.g., baseline, follow-up etc.
#'
#' @param digits_cont Digits for summary statistics and CI of continuous
#' variables. Default to 1.
#'
#' @param digits_cat Digits for summary statistics and CI of categorical
#' variables. Default to 1.
#'
#' @param missing Logical. If TRUE (default), the missing values are shown.
#'
#' @param missing_percent Indicates whether percentages for missings are shown
#' (TRUE, default)
#' or not (FALSE) for categorical variables.
#'  If "both", then both options are displayed next to each other.
#'
#' @param missing_text String indicating text shown on missing row. Default to
#' "Missing".
#'
#' @param add_n Logical. If TRUE, an additional column with the total
#' number of non-missing observations for each variable is added.
#'
#' @param overall Logical. If TRUE, an additional column with the total is
#' added to the table. Ignored, if no groups are defined. Default to FALSE.
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
#' @return A table of class "`flextable`" or `c("tbl_strata_nested_stack", "tbl_stack", "gtsummary")`.
#' Optionally returns a .docx file in the specified folder.
#'
#' @import cardx dplyr gtsummary forcats purrr
#' @importFrom Hmisc label
#' @importFrom stats sd t.test
#' @export


summaryByVisit<- function(data,
                          vars = NULL,
                          group = NULL,
                          labels = NULL,
                          stat_cont = "median_range",
                          stat_cat = "n_percent",
                          visit = "visit",
                          order = NULL,
                          visitgroup = NULL,
                          digits_cont=1,
                          digits_cat=1,
                          missing_percent=TRUE,
                          missing=TRUE,
                          missing_text = "Missing",
                          add_n = FALSE,
                          overall = FALSE,
                          as_flex_table = FALSE,
                          border = TRUE,
                          word_output = FALSE,
                          file_name = paste0("SummaryByVisit_", format(Sys.Date(), "%Y%m%d"), ".docx")){
  tbl_out <- NULL
  for (v in 1:length(vars)){
  if (is.numeric(data[[vars[[v]]]])==TRUE){
    tbl0 <- summaryByVisitContinuous(data,
                   vars = vars[[v]],
                   group = group,
                   labels = labels[[v]],
                   stat_cont = stat_cont,
                   visit = visit,
                   order = order,
                   visitgroup = visitgroup,
                   digits_cont = digits_cont,
                   add_n = add_n,
                   overall = overall,
                   as_flex_table = FALSE,
                   border = border,
                   word_output = word_output,
                   file_name = file_name)
  }
  else{
    tbl0 <- summaryByVisitCategorical(data,
                                         vars = vars[[v]],
                                         group = group,
                                         labels = labels[[v]],
                                         stat_cat = stat_cat,
                                         visit = visit,
                                         order = order,
                                         visitgroup = visitgroup,
                                         digits_cat = digits_cat,
                                         missing_percent = missing_percent,
                                         missing = missing,
                                         missing_text = missing_text,
                                         add_n = add_n,
                                         overall = overall,
                                         as_flex_table = FALSE,
                                         border = border,
                                         word_output = word_output,
                                         file_name = file_name)

  }
    tbl0$table_body$tbl_indent_id1 <-as.numeric(tbl0$table_body$tbl_indent_id1)
    tbl_out <- if (is.null(tbl_out)) {
      tbl0
    } else {
      tbl_stack(list(tbl_out, tbl0))
    }
  }
  #

  # if flex_table is needed
  if(as_flex_table == TRUE | word_output == TRUE){
    if (border == TRUE){
      tbl_print <- FitFlextableToPage(gtsummary::as_flex_table(tbl_out)|>
                                        flextable::border_outer(part = "header")|>
                                        flextable::border_outer(part = "body") )
    } else {
      tbl_print <- FitFlextableToPage(gtsummary::as_flex_table(tbl_out))
    }
  } else {
    tbl_print <- tbl_out
  }


  if (word_output == TRUE) {

    # Create Word document
    doc <- officer::read_docx()
    doc <- flextable::body_add_flextable(doc, value = tbl_print)

    # Save to specified location
    print(doc, target = file_name)

    message("Table saved to: ", normalizePath(file_name))
  }



  return(tbl_print)
}
