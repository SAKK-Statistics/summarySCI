#' Creates publication-ready summary tables
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
#' Options include "t.test", "oneway.test", "kruskal.test", "wilcox.test" (default),
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
#' be "continous" (default) or "categorical".
#'
#' @param dichotomous_as Type for the dichotomous variables. Can either be
#' "categorical" (default, one row per level) or "dichotomous" (only
#' one row with reference level (see argument `value`), only works if `missing = "FALSE"` or
#' `missing_percent = FALSE`.
#'
#' @param value Specifies the reference level of a variable to display on a single row.
#' Default is NULL. The syntax is as follows: `value = list(varname ~ "level to show")`.
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


summaryTable <- function(data,
                         vars = NULL,
                         group = NULL,
                         labels = NULL,
                         stat_cont = "median_range",
                         stat_cat = "n_percent",
                         continuous_as = "continuous",
                         dichotomous_as = "dichotomous",
                         value = NULL,
                         test = FALSE,
                         test_cont = "wilcox.test",
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

  # --------- Some checks --------------------------------------------------- #

  # Make sure that 'data' exists and that it is a data frame
  if (missing(data)) {
    stop("'data' must be specified.")
  }
  data <- as.data.frame(data)


  # test is TRUE only if group is given.
  if(is.null(group) & test == TRUE){
    stop("Error: 'group' needs to be given for a test to be calculated.")
  }



  #  -----------------------------------------------------------------------------

  # if vars = NULL, take all the variables (except group if not NULL).
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }


  # Summary stat for continuous variables
  stat_cont <- format_lookup[[stat_cont]]
  stat_cat <- format_lookup_cat[[stat_cat]]


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






if(is.null(labels)){
labels <- get_labels(data, vars)
}


## Trick for fisher.test as default and if NULL, take the default.

  if(test == TRUE){
test_list <- list(all_continuous() ~ test_cont)

if (!is.null(test_cat)) {
    test_list <- c(test_list, all_categorical() ~ test_cat)
}

}






  # -------------- # table for missing = FALSE (default in gtsummary)----- #
  if(missing_percent == FALSE | missing == FALSE){


    if(missing == FALSE){
      missing_var <-  "no"
    } else {
      missing_var <-  "ifany"
    }

    # we might need to code twice.
    # We want to identify continuous variables with more than
    # two unique values and treat them as continuous (and not factors)

    # Identify numeric variables
    numeric_vars <- intersect(vars, names(data)[sapply(data, is.numeric)])

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

  tbl_noMissing <- tbl_summary(data = data,
                               include = all_of(vars),
                               by = group,
                               type = type,
                               value = value,
                     label = labels,
                     statistic = list(all_continuous() ~ stat_cont,
                                      all_categorical() ~ stat_cat),
                     missing_text = missing_text,
                     missing = missing_var,
                     digits = list(all_categorical() ~ digits_cat,
                                   all_continuous() ~ digits_cont))


  # need that for the add_n()

  tbl_for_add_n <- tbl_noMissing %>% add_n()

  if(!is.null(group)){
   tbl_for_add_n <-  tbl_noMissing %>%
      add_stat(
        fns = everything() ~ add_by_n
      ) %>%
      modify_header(starts_with("add_n_stat") ~ "**N**") %>%
      modify_table_body(
        ~ .x %>%
          dplyr::relocate(add_n_stat_1, .before = stat_1) %>%
          dplyr::relocate(add_n_stat_2, .before = stat_2)
      )
  }


  if(test == TRUE){


    tbl_noMissing <-  tbl_noMissing|>
      add_p(pvalue_fun = label_style_pvalue(digits = 2),
            test = test_list)

}

  if(ci == TRUE){
    tbl_noMissing <- tbl_noMissing |>
      add_ci(method = list(all_continuous() ~ ci_cont,
                           all_categorical() ~ ci_cat_gt),
             conf.level = conf_level,
             statistic = list(all_continuous() ~ "[{conf.low}, {conf.high}]",
                              all_categorical() ~ "[{conf.low}%, {conf.high}%]"))

}


# --------------------------  missing = FALSE -------------------------------- #
tbl <- tbl_noMissing

if(overall == TRUE & !is.null(group)){
  tbl <- tbl %>%
    add_overall(last = TRUE)
}
}
# --------------------------  missing = TRUE --------------------------------- #

if(missing_percent != FALSE & missing != FALSE){

  data2 <- data
  colnames(data2) <-  colnames(data)

    for (i in colnames(data2|>
                       dplyr::select(all_of(c(vars, group))))) {

      if (is.factor(data2[[i]]) == TRUE | is.character(data2[[i]])) {
        data2[[i]] <- forcats::fct_na_value_to_level(as.factor(data2[[i]]), level = missing_text)
        # data2[[i]] <- forcats::fct_explicit_na(as.factor(data2[[i]]), na_level = missing_text)
        if (!is.null(attr(data[[i]], "label"))) {
          Hmisc::label(data2[[i]]) <- attr(data[[i]], "label")
        }
      } else if (all(data2[[i]] %in% c(0, 1, NA))) {
         data2[[i]] <- forcats::fct_na_value_to_level(factor(data2[[i]]), level = missing_text)
        # data2[[i]] <- forcats::fct_explicit_na(factor(data2[[i]]))
        if (!is.null(attr(data[[i]], "label"))) {
          Hmisc::label(data2[[i]]) <- attr(data[[i]], "label")
        }
      }
    }

  data2 <- droplevels(data2)

  # We want to identify continuous variables with more than
  # two unique values and treat them as continuous (and not factors)

  # Identify numeric variables
  numeric_vars <- intersect(vars, names(data2)[sapply(data2, is.numeric)])

  if (length(numeric_vars) == 0) {
    dichotomous_vars <- character(0)
    continuous_vars <- character(0)
  } else {
    # Find dichotomous (binary) numeric variables
    dichotomous_vars <- numeric_vars[
      sapply(data2[numeric_vars], function(x) {
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



    tbl_missing <- data2|>
      tbl_summary(by = group,
                  label = labels,
                  include = all_of(vars),
                  type = type,
                  value = value,
                  statistic = list(all_continuous() ~ stat_cont,
                                   all_categorical() ~ stat_cat),
                  missing_text = missing_text,
                  digits = list(all_categorical() ~ digits_cat,
                                all_continuous() ~ digits_cont))

    # need that for the add_n()

    tbl_for_add_n <- data|>
      tbl_summary(by = group,
                  label = labels,
                  include = all_of(vars),
                  type = type,
                  value = value,
                  statistic = list(all_continuous() ~ stat_cont,
                                   all_categorical() ~ stat_cat),
                  missing_text = missing_text,
                  digits = list(all_categorical() ~ digits_cat,
                                all_continuous() ~ digits_cont)) %>% add_n()

      if(!is.null(group)){
        tbl_for_add_n <-  data|>
          tbl_summary(by = group,
                      label = labels,
                      include = all_of(vars),
                      type = type,
                      value = value,
                      statistic = list(all_continuous() ~ stat_cont,
                                       all_categorical() ~ stat_cat),
                      missing_text = missing_text,
                      digits = list(all_categorical() ~ digits_cat,
                                    all_continuous() ~ digits_cont)) %>%
          add_stat(
            fns = everything() ~ add_by_n
          ) %>%
          modify_header(starts_with("add_n_stat") ~ "**N**") %>%
          modify_table_body(
            ~ .x %>%
              dplyr::relocate(add_n_stat_1, .before = stat_1) %>%
              dplyr::relocate(add_n_stat_2, .before = stat_2)
          )
      }



    if(ci == TRUE) {
      tbl_missing <- tbl_missing |>
        add_ci(method = list(all_continuous() ~ ci_cont,
                             all_categorical() ~ ci_cat_gt),
               conf.level = conf_level,
               statistic = list(all_continuous() ~ "[{conf.low}, {conf.high}]",
                                (all_categorical() ~ "[{conf.low}%, {conf.high}%]")))
    }



      # tests displayed (!missings not counted in calculation!)
    # -> only take p-value from other table
    if (test == TRUE) {
      tbl_noMissing_short <- tbl_summary(data = data,
                                         label = labels,
                                         include = all_of(vars),
                                         type = type,
                                         value = value,
                                   missing = "no",
                                   missing_text = missing_text,
                                   by = group,
                                   statistic = list(all_categorical() ~ stat_cat),
                                   digits = list(all_categorical() ~ digits_cat)
      ) |>
        add_p(pvalue_fun = label_style_pvalue(digits = 2),
              test = test_list) |>
        modify_column_hide(c("stat_1", "stat_2"))

      if(overall == TRUE & !is.null(group)){
          tbl_noMissing_short <- tbl_noMissing_short %>%
            add_overall(last = TRUE)
        }
    }


# merging table with missings and p-value
    if(test == TRUE){
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

  if(missing_percent == TRUE & missing != FALSE){
    tbl <- tbl_missingTRUE
  }



if(missing_percent == "both" & missing != FALSE){
  tbl_noMissing2 <- tbl_summary(data = data,
                                include = all_of(vars),
                               label = labels,
                               by = group,
                               type = type,
                               value = value,
                               statistic = list(all_continuous() ~ stat_cont,
                                                all_categorical() ~ stat_cat),
                               missing = "no",
                               missing_text = missing_text,
                               digits = list(all_categorical() ~ digits_cat,
                                             all_continuous() ~ digits_cont))


if(test == TRUE){
  tbl_both <- tbl_merge(tbls = list(tbl_missing, tbl_noMissing2, tbl_noMissing_short)) |>
    modify_spanning_header(c("stat_1_1", "stat_2_1") ~ "**With missing**",
                           c("stat_1_2", "stat_2_2") ~ "**Without missing**",
                           c("p.value_3") ~ "")
}

  if(test == FALSE){
    tbl_both <- tbl_merge(tbls = list(tbl_missing, tbl_noMissing2))|>
      modify_spanning_header(c("stat_1_1", "stat_2_1" ) ~ "**With missing**",
                             c("stat_1_2", "stat_2_2") ~ "**Without missing**")
  }
  tbl <- tbl_both
}



  if(add_n == TRUE & is.null(group)){

    # Step 1: Extract n values from the reference table
    n_values <- tbl_for_add_n$table_body %>%
      filter(row_type == "label") %>%
      select(variable, n) %>%
      rename(n_custom = n)

    # Step 2: Add n_custom as a new column and place it next to the variable name
    tbl <- tbl %>%
      modify_table_body(
        ~ .x %>%
          left_join(n_values, by = "variable") %>%
          mutate(N = ifelse(row_type == "label", as.character(n_custom), NA)) %>%
          relocate(N, .after = label)  # This moves N right after label
      ) %>%
      modify_table_styling(columns = "N") %>%
      modify_header(N ~ "N") %>%
      modify_column_alignment(columns = "N", align = "center")

  }



  if(add_n == TRUE & !is.null(group)){


  # Step 1: Extract group-specific n values from the reference table

  n_values <- tbl_for_add_n$table_body %>%
    filter(row_type == "label") %>%
    select(variable, add_n_stat_1 = add_n_stat_1, add_n_stat_2 = add_n_stat_2)


  if(test == FALSE & missing_percent != "both" | missing_percent == FALSE | missing == FALSE){
  tbl <- tbl %>%
    modify_table_body(
      ~ .x %>%
        left_join(n_values, by = "variable") %>%
        mutate(
          add_n_stat_1 = ifelse(row_type == "label", as.character(add_n_stat_1), NA),
          add_n_stat_2 = ifelse(row_type == "label", as.character(add_n_stat_2), NA)
        ) %>%
        relocate(add_n_stat_1, .before = stat_1) %>%
        relocate(add_n_stat_2, .before = stat_2)
    ) %>%
    modify_header(
      add_n_stat_1 ~ "**N**",
      add_n_stat_2 ~ "**N**"
    ) %>%
    modify_column_alignment(columns = c("add_n_stat_1", "add_n_stat_2"), align = "center") %>%
    modify_table_styling(columns = c("add_n_stat_1", "add_n_stat_2"), footnote = "N without missing values")

  } else if (test == TRUE & missing_percent == TRUE) {
  tbl <- tbl %>%
    modify_table_body(
      ~ .x %>%
        left_join(n_values, by = "variable") %>%
        mutate(
          add_n_stat_1 = ifelse(row_type == "label", as.character(add_n_stat_1), NA),
          add_n_stat_2 = ifelse(row_type == "label", as.character(add_n_stat_2), NA)
        ) %>%
        relocate(add_n_stat_1, .before = stat_1_1) %>%
        relocate(add_n_stat_2, .before = stat_2_1)
    ) %>%
    modify_header(
      add_n_stat_1 ~ "**N**",
      add_n_stat_2 ~ "**N**"
    ) %>%
    modify_column_alignment(columns = c("add_n_stat_1", "add_n_stat_2"), align = "center") %>%
    modify_table_styling(columns = c("add_n_stat_1", "add_n_stat_2"), footnote = "N without missing values")

  }
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

