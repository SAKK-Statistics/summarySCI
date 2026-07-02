#' Creates publication-ready summary tables (new internal structure)
#'
#' Creates publication-ready summary tables based on the gtsummary
#' package.
#'
#' @param data A data frame or tibble containing the data to be summarized.
#'
#' @param vars Variables to include in the summary table.
#' Need to be specified with quotes, e.g. `"age"` or `c("age", "response")`.
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
#' @param stat_cont Summary statistic to display for continuous variables. Options
#' include "median_IQR", "median_range" (default), "mean_sd", "mean_se" and
#' "geomMean_sd".
#'
#' @param stat_cat Summary statistic to display for categorical variables.
#' Options include "n_percent" (default) and "n", and "n_N".
#'
#' @param test Logical. Indicates whether p-values are displayed (TRUE)
#' or not (FALSE). Default to FALSE
#'
#' @param test_cont Test type used to calculate the p-value
#' for continuous variables. Only used if `test = TRUE`.
#' Options include "t.test", "oneway.test", "kruskal.test" (default for
#' more than two groups), "wilcox.test" (default for two
#' groups),
#' "paired.t.test", "paired.wilcox.test"
#'
#' @param test_cat Test type used to calculated the p-value
#' for categorical variables.  Only used if `test = TRUE`.
#' Options include "fisher.test" (default), "chisq.test", "chisq.test.no.correct".
#' If NULL, the function decides itself: "chisq.test.no.correct" for categorical
#' variables with all expected
#' cell counts >=5, and "fisher.test" for categorical variables with
#' any expected cell count <5.
#'
#' @param continuous_as Type for the continuous variables. Can either
#' be "continuous" (default) or "categorical".
#'
#' @param dichotomous_as Type for the dichotomous variables. Can either be
#' "categorical" (default, one row per level) or "dichotomous" (only
#' one row with reference level (see argument `ref_level`), only works if `missing = "FALSE"` or
#' `missing_percent = FALSE`.
#'
#' @param ref_level Specifies the reference level of a variable to display on a single row.
#' Default is the first appearing level. The syntax is as follows: `ref_level = list(varname ~ "level to show")`.
#'
#' @param ci Logical. Indicates whether CI are displayed (TRUE) or
#' not (FALSE). Default to FALSE.
#'
#' @param ci_cont Confidence interval method for continuous variables.
#'  Only used if `ci = TRUE`.
#' Options include "t.test" and "wilcox.test" (default).
#'
#' @param ci_cat Confidence interval method for categorical variables.
#' Options include "wilson" (default), "wilson.no.correct", "clopper.pearson",
#' "wald", "wald.no.correct", "agresti.coull" and "jeffreys".
#' If NULL, no CI will be displayed.
#'
#' @param conf_level Numeric. Confidence level. Default to 0.95.
#'
#' @param digits_cont Numeric. Digits for summary statistics and CI of continuous
#' variables. Default to 1.
#'
#' @param digits_cat Numeric. Digits for summary statistics and CI of categorical
#' variables. Default to 0.
#'
#' @param missing Logical. If TRUE (default), the missing values are shown.
#'
#'
#' @param missing_percent Indicates whether percentages for missings are shown
#' (TRUE, default)
#' or not (FALSE) for categorical variables.
#'  If "both", then both options are displayed next to each other.
#'
#' @param missing_text String indicating text shown on missing row. Default to
#' "Missing".
#'
#' @param overall Logical. If TRUE, an additional column with the total is
#' added to the table. Default to FALSE.
#'
#' @param add_n Logical. If TRUE (default), an additional column with the total
#' number of non-missing observations for each variable is added.
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
#' @return A table of class "`flextable`" or `c("tbl_summary", "gtsummary")`.
#' Optionally returns a .docx file in the specified folder.
#'
#' @examples
#'
#' library(survival)
#' data("cancer")
#' summaryTable(data = cancer,vars = c("inst", "time","age", "ph.ecog"),
#'              labels = list(inst = "Institution code",
#'                            time = "Time",
#'                            age = "Age",
#'                            ph.ecog = "ECOG score"))
#' @import cardx dplyr gtsummary forcats
#' @importFrom Hmisc label
#' @importFrom stats sd t.test na.omit
#' @importFrom flextable autofit width flextable_dim
#' @importFrom officer read_docx
#' @export


summaryTable_new <- function(data,
                         vars = NULL,
                         group = NULL,
                         labels = NULL,
                         stat_cont = "median_range",
                         stat_cat = "n_percent",
                         continuous_as = "continuous",
                         dichotomous_as = "dichotomous",
                         ref_level = NULL,
                         test = FALSE,
                         test_cont = NULL,
                         test_cat = "fisher.test",
                         ci = FALSE,
                         ci_cont = "wilcox.test",
                         ci_cat = "wilson",
                         conf_level = 0.95,
                         digits_cont = 1,
                         digits_cat = 0,
                         missing = TRUE, ## !
                         missing_percent = TRUE,
                         missing_text = "Missing",
                         overall = FALSE,
                         add_n = TRUE,
                         as_flex_table = TRUE,
                         border = TRUE,
                         word_output = FALSE,
                         file_name = paste0("SummaryTable_", format(Sys.Date(), "%Y%m%d"), ".docx")){

################################################################################
# Settings and input checks ----------------------------------------------------
################################################################################

  ## data exists and is df -----
  if (missing(data)) {
    stop("'data' must be specified.")
  }
  data <- as.data.frame(data)


  ## test is TRUE only if group is given -----
  if(is.null(group) & test == TRUE){
    stop("Error: 'group' needs to be given for a test to be calculated.")
  }

  ## if missing_percent is both, missing is TRUE -----
  if(missing == FALSE & missing_percent == "both"){
    missing == TRUE
  }

  ## if missing percent is both, dichotomous_as is categorical -----
  if( missing_percent == "both"){
   dichotomous_as = "categorical"
  }

  ## if missing is FALSE, var_missing is no -----
  var_missing <- ifelse(missing == FALSE, "no", "ifany")


  ## if vars = NULL, take all the variables -----
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }

  ## group as factor -----
  if(!is.null(group)) data[[group]] <- as.factor(data[[group]])


  ## summary stat for continuous and categorical variables -----
  stat_cont <- format_lookup[[stat_cont]]
  stat_cat <- format_lookup_cat[[stat_cat]]


  ## type of CI for cat variables -----
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


  ## labels -----
  if(is.null(labels)){
    labels <- get_labels(data, vars)
  }

  ## test for continuous variables ----

    if(length(unique(data[, group])) == 2 & is.null(test_cont)){
      test_cont = "wilcox.test"
    }

    if(length(unique(data[, group])) > 2 & is.null(test_cont)){
      test_cont = "kruskal.test"
    }

    test_list <- list(all_continuous() ~ test_cont)

  ## test for categorical variables -----

    if (!is.null(test_cat)) {
      test_list <- c(test_list, all_categorical() ~ test_cat)
    }


  ## if no group, add a dummy group
  if(is.null(group) == TRUE){
    data$dummygroup = "Overall"
    group <- "dummygroup"
  }

################################################################################
# data_missing_as_level ----------------------------------------------------
################################################################################

## dataset where missing values are considered a level
# (for missing_percent = "both")

data_missing_as_level <- data

colnames(data_missing_as_level) <-  colnames(data)

## make missing a level -----

      for (i in colnames(data_missing_as_level|>
                         dplyr::select(all_of(c(vars))))) {

        if (is.factor(data_missing_as_level[[i]]) == TRUE | is.character(data_missing_as_level[[i]])) {
          data_missing_as_level[[i]] <- forcats::fct_na_value_to_level(as.factor(data_missing_as_level[[i]]), level = missing_text)
          if (!is.null(attr(data[[i]], "label"))) {
            Hmisc::label(data_missing_as_level[[i]]) <- attr(data[[i]], "label")
          }
        } else if (all(data_missing_as_level[[i]] %in% c(0, 1, NA))) {
          data_missing_as_level[[i]] <- forcats::fct_na_value_to_level(factor(data_missing_as_level[[i]]), level = missing_text)
          if (!is.null(attr(data[[i]], "label"))) {
            Hmisc::label(data_missing_as_level[[i]]) <- attr(data[[i]], "label")
          }
        }
      }

data_missing_as_level <- droplevels(data_missing_as_level)

## identify numeric variables -----
numeric_vars_2 <- intersect(vars, names(data_missing_as_level)[sapply(data_missing_as_level, is.numeric)])

## identify dichotomous and continuous variables -----
if (length(numeric_vars_2) == 0) {
  dichotomous_vars_2 <- character(0)
  continuous_vars_2 <- character(0)
} else {
  # Find dichotomous (binary) numeric variables
  dichotomous_vars_2 <- numeric_vars_2[
    sapply(data_missing_as_level[numeric_vars_2], function(x) {
      values <- sort(unique(na.omit(x)))
      length(values) == 2 && all(values == c(0, 1))
    })
  ]

  ### Continuous variables = numeric minus binary
  continuous_vars_2 <- setdiff(numeric_vars_2, dichotomous_vars_2)
}


## set the type of variable for data_missing_as_level
type_missing_as_level <- list()

### Append continuous variable types if any
if (length(continuous_vars_2) > 0) {
  type_missing_as_level <- append(type_missing_as_level, list(all_of(continuous_vars_2) ~ continuous_as))
}

### Append dichotomous variable types if any
if (length(dichotomous_vars_2) > 0) {
  type_missing_as_level <- append(type_missing_as_level, list(all_of(dichotomous_vars_2) ~ dichotomous_as))
}




      #### - !!!! ##

      # We want to identify continuous variables with more than
      # two unique values and treat them as continuous (and not factors)

      # Identify numeric variables
      # numeric_vars <- intersect(vars, names(data_missing_as_level)[sapply(data_missing_as_level, is.numeric)])
      numeric_vars <- intersect(vars, names(data)[sapply(data, is.numeric)])


# 2X zu haben
      # 1x für data und 1x für data_missing_as_level
      if (length(numeric_vars) == 0) {
        dichotomous_vars <- character(0)
        continuous_vars <- character(0)
      } else {
        # Find dichotomous (binary) numeric variables
        dichotomous_vars <- numeric_vars[
          sapply(data[numeric_vars], function(x) {
            values <- sort(unique(na.omit(x)))
            length(values) == 2 && all(values == c(0, 1))
          })
        ]

        # Continuous variables = numeric minus binary
        continuous_vars <- setdiff(numeric_vars, dichotomous_vars)
      }

      type <- list()

      # Append continuous variable types if any
      if (length(continuous_vars) > 0) {
        type <- append(type, list(all_of(continuous_vars) ~ continuous_as))
      }

      # Append dichotomous variable types if any
      if (length(dichotomous_vars) > 0) {
        type <- append(type, list(all_of(dichotomous_vars) ~ dichotomous_as))
      }





# missing should always be no in missing table when merged
   var_missing <- ifelse(missing_percent == "both",
                         "no",
                         var_missing)

## Table without missing or with missing but without percent -----
      tbl_noMissing_default <- gtsummary::tbl_summary(data = data,
                                           include = all_of(vars),
                                           label = labels,
                                           by = group,
                                           type = type,
                                           value = ref_level,
                                           statistic = list(all_continuous() ~ stat_cont,
                                                            all_categorical() ~ stat_cat),
                                           missing = var_missing,
                                           missing_text = missing_text,
                                           digits = list(all_categorical() ~ digits_cat,
                                                         all_continuous() ~ digits_cont)) |>

        add_ci(method = list(all_continuous() ~ ci_cont,
                             all_categorical() ~ ci_cat_gt),


               style_fun = list(
                 all_continuous() ~ purrr::partial(style_number, digits = digits_cont),
                 all_categorical() ~ purrr::partial(style_percent, digits = digits_cat)
               ),


               conf.level = conf_level,
               statistic = list(all_continuous() ~ "[{conf.low}, {conf.high}]",
                                (all_categorical() ~ "[{conf.low}%, {conf.high}%]")))|>
    add_stat(
      fns = everything() ~ add_by_n
    ) %>%
    modify_header(starts_with("add_n_stat") ~ "**N**") %>%
    modify_table_body(
      ~ reduce(
        .x = seq_len(length(unique(na.omit(data[, group])))),
        .init = .x,
        .f = ~ relocate(
          .x,
          !!paste0("add_n_stat_", .y),
          .before = !!paste0("stat_", .y)
        )
      )
    ) %>%
        modify_table_styling(columns = c(starts_with("add_n_stat_")), footnote = "N without missing values")

      # add foot note

  # Step 1: Extract n values from the reference table

  n_values <- tbl_noMissing_default$table_body %>%
    filter(row_type == "label") %>%
    select(variable, starts_with("add_n_stat_"))



      tbl_missing_percent <- data_missing_as_level|>
        gtsummary::tbl_summary(by = group,
                               label = labels,
                               include = all_of(vars),
                                type = type_missing_as_level,
                               value = ref_level,
                               statistic = list(all_continuous() ~ stat_cont,
                                                all_categorical() ~ stat_cat),
                               missing_text = missing_text,
                               digits = list(all_categorical() ~ digits_cat,
                                             all_continuous() ~ digits_cont)) |>
        add_ci(method = list(all_continuous() ~ ci_cont,
                             all_categorical() ~ ci_cat_gt),

               style_fun = list(
                 all_continuous() ~ purrr::partial(style_number, digits = digits_cont),
                 all_categorical() ~ purrr::partial(style_percent, digits = digits_cat)
               ),
               conf.level = conf_level,
               statistic = list(all_continuous() ~ "[{conf.low}, {conf.high}]",
                                (all_categorical() ~ "[{conf.low}%, {conf.high}%]")))  %>%
        modify_table_body(
          ~ .x %>%
            left_join(n_values, by = "variable") %>%
            mutate(
              across(
                starts_with("add_n_stat_"),
                ~ ifelse(row_type == "label", as.character(.x), NA_character_)
              )
            ) ) %>%

        modify_column_alignment(columns = c(starts_with("add_n_stat_")), align = "center") %>%
        modify_table_styling(columns = c(starts_with("add_n_stat_")), footnote = "N without missing values") %>%
        modify_header(starts_with("add_n_stat") ~ "**N**") %>%

        modify_table_body(
          ~ reduce(
            .x = seq_len(length(unique(na.omit(data[, group])))),
            .init = .x,
            .f = ~ relocate(
              .x,
              !!paste0("add_n_stat_", .y),
              .before = !!paste0("stat_", .y)
            )
          )
        )






      ### Test == TRUE -----
      # tests displayed (!missings not counted in calculation!)
      # -> only take p-value from other table

      if(group != "dummygroup"){

        tbl_noMissing_short <- gtsummary::tbl_summary(data = data,
                                                      label = labels,
                                                      include = all_of(vars),
                                                      type = type,
                                                      value = ref_level,
                                                      missing = "no",
                                                      # missing = var_missing,
                                                      missing_text = missing_text,
                                                      by = group,
                                                      statistic = list(all_categorical() ~ stat_cat),
                                                      digits = list(all_categorical() ~ digits_cat)
        ) |>
          add_p(pvalue_fun = label_style_pvalue(digits = 2),
                test = test_list) |>
          modify_column_hide(starts_with("stat_"))
        # %>%
    # add_n(last = TRUE) %>%
    # add_overall(last = TRUE) %>%
    # modify_footnote_header(
    #   columns  = n,
    #   footnote = "N without missing values"
    # )

}

  #
  #     if(group != "dummygroup"){
  #
  # tbl_both <- tbl_merge(tbls = list(tbl_missing_percent, tbl_noMissing_default, tbl_noMissing_short)) |>
  #   modify_spanning_header(c(starts_with("stat_") & ends_with("_1")) ~ "**With missing**",
  #                          c(starts_with("stat_") & ends_with("_2")) ~ "**Without missing**",
  #                          ## TO DO: fix header with missing
  #                          c("p.value_3") ~ "",
  #                          starts_with("n_") ~ "",
  #                          starts_with("stat_0_") ~ "")
  #     } else {
  #       tbl_both <- tbl_merge(tbls = list(tbl_missing_percent, tbl_noMissing_default)) |>
  #         modify_spanning_header(c(starts_with("stat_") & ends_with("_1")) ~ "**With missing**",
  #                                c(starts_with("stat_") & ends_with("_2")) ~ "**Without missing**")
  #     }

# returned table -----


#######################################################################3

      # 1.
    if(missing_percent == "both"){

      if(overall == TRUE){

        tbl_return <-  tbl_merge(tbls = list(tbl_missing_percent, tbl_noMissing_default %>%
                                               add_n(last = TRUE) %>%
                                             add_overall(last = TRUE)))

      } else{
     tbl_return <-  tbl_merge(tbls = list(tbl_missing_percent, tbl_noMissing_default))
      }
    }

      #2.
      	if(missing_percent == FALSE | missing == FALSE){

      	  if(overall == TRUE){
      	  tbl_return <- tbl_noMissing_default %>%
      	    add_n(last = TRUE) %>%
      	    add_overall(last = TRUE) %>%
      	    modify_table_styling(columns = c(starts_with("n_")), footnote = "N without missing values")

      	  } else {
      	    tbl_return <- tbl_noMissing_default
      	  }
      	}

      # 3.
      if(missing_percent == TRUE){

        if(overall == TRUE){
          tbl_return <- tbl_missing_percent %>%
            add_n(last = TRUE) %>%
            add_overall(last = TRUE) %>%
            modify_table_styling(columns = c(starts_with("n_")), footnote = "N without missing values")
        } else{


        tbl_return <- tbl_missing_percent
        }
      }


      # 4.
      if(test == TRUE){


        tbl_return <- tbl_merge(list(tbl_return, tbl_noMissing_short))
        # %>%
          # remove_spanning_header()
      }

      #5.
        if(ci == FALSE){
           tbl_return <- tbl_return%>%
             modify_column_hide(starts_with("ci_"))
        }




        #7
        if(add_n == FALSE){
          tbl_return <- tbl_return%>%
            modify_column_hide(starts_with("n") | starts_with("add_n"))
        }

      tbl_return <- tbl_return %>%
        remove_spanning_header()

      if(missing_percent == "both")
        tbl_return <- tbl_return %>%
        modify_spanning_header(c(#starts_with("stat_") &
          ends_with("_1")) ~ "**With missing**",
          c(#starts_with("stat_") &
            ends_with("_2") | ends_with("_2_1")) ~ "**Without missing**") %>%
        modify_table_styling(columns = c(starts_with("n_")), footnote = "N without missing values")


      return(tbl_return)

}

