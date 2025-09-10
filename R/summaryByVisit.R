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
#' @import cardx dplyr gtsummary forcats purrr
#' @importFrom Hmisc label
#' @importFrom stats sd t.test
#' @export


summaryByVisit<- function(data,
                          vars = NULL,
                          group = NULL,
                          labels = NULL,
                          stat_cont = "median_range",
                          visit = "visit",
                          order = NULL,
                          visitgroup = NULL,
                          digits_cont=1,
                          add_n = FALSE,
                          overall = FALSE,
                          as_flex_table = TRUE,
                          border = TRUE){

  # --------- Some checks --------------------------------------------------- #

  # Make sure that 'data' exists and that it is a data frame
  if (missing(data)) {
    stop("'data' must be specified.")
  }

  # stop if more than 3 groups are requested
  if (!is.null(group)){
    if (length(unique(data[[group]]))>3){
      stop("'A maximum of 3 groups are currently supported'")
    }
  }


  if(is.null(labels)){
    labels <- get_labels(data = data, vars = vars)
  }

  # ---------------------------------------------------- #
  # settle visit order
  if (!is.null(order)){
    data <- data|>
      dplyr::arrange(order)|>
      dplyr::mutate(visit = factor(visit, levels = unique(visit)))|>
      as.data.frame()
  } else{
    # order visit numbers not lexicographic
    data <- data|>
      dplyr::mutate(group_num = as.numeric(gsub("[^0-9]", "", visit)))|>
      dplyr::arrange(group_num)|>
      dplyr::mutate(visit = factor(visit, levels = unique(visit)))|>
      as.data.frame()
  }

  # remove rows without visit
  data <- data[(!is.na(data[[visit]])),]
  data <- data[(data[[visit]] != ""),]


  # Summary stat for continuous variables
  stat_cont <- format_lookup[[stat_cont]]

  # if vars = NULL, take all the variables (except group if not NULL).
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }


  if (!all(sapply(data[vars], is.numeric))) {
    stop("'All vars must be numeric'")
  }



  tbl<-NULL

  for (i in 1:length(vars)){

    # without visitgroup
    if (is.null(visitgroup)){
      strata0=visit
      indent=1
      select_vars=c(visit, vars[i])
    }
    # with visitgroup
    else{
      strata0=c(visitgroup, visit)
      indent=2
      select_vars=c(visitgroup, visit, vars[i])
    }
    ### create nested table
    # Without groups
    if (is.null(group)){
      assign(paste0("t", i), data|>
               dplyr::select(select_vars)|>
               gtsummary::tbl_strata_nested_stack(
                 .x ,
                 strata = strata0,
                 .tbl_fun = ~ .x |>
                   gtsummary::tbl_summary(missing="no",
                                          statistic = list(gtsummary::all_continuous() ~ stat_cont),
                                          type= vars[i] ~ "continuous",
                                         digits = list(gtsummary::all_continuous() ~ digits_cont))|>
                   gtsummary::add_n()|>
                   gtsummary::add_overall()|>
                   gtsummary::modify_header(update = list(label ~ paste0("**", gsub("\\b(\\w)", "\\U\\1", tolower(visit), perl = TRUE),"**"))), quiet = TRUE)
      )
    }
    # for 2 groups
    else {
      if (length(unique(data[[group]]))==2){
        assign(paste0("t", i), data|>
                 dplyr::select(select_vars, group)|>
                 gtsummary::tbl_strata_nested_stack(
                   .x ,
                   strata = strata0,
                   .tbl_fun = ~ .x |>
                     gtsummary::tbl_summary(missing="no",
                                            statistic = list(gtsummary::all_continuous() ~ stat_cont),
                                            by=group,
                                            type= vars[i] ~ "continuous",
                                           digits = list(gtsummary::all_continuous() ~ digits_cont))|>
                     gtsummary::add_n()|>
                     gtsummary::add_overall()|>
                     gtsummary::add_stat(
                       fns = dplyr::everything() ~ add_by_n
                     ) |>
                     gtsummary::modify_header(starts_with("add_n_stat") ~ "**N**") |>
                     gtsummary::modify_table_body(
                       ~ .x |>
                         dplyr::relocate(n, .before = stat_0) |>
                         dplyr::relocate(add_n_stat_1, .before = stat_1) |>
                         dplyr::relocate(add_n_stat_2, .before = stat_2)
                     )|>
                     gtsummary::modify_header(update = list(label ~ paste0("**", gsub("\\b(\\w)", "\\U\\1", tolower(visit), perl = TRUE),"**"))), quiet = TRUE)
        )
      }
      # for 3 groups
      if (length(unique(data[[group]]))==3){
        assign(paste0("t", i), data|>
                 dplyr::select(select_vars, group)|>
                 gtsummary::tbl_strata_nested_stack(
                   .x ,
                   strata = strata0,
                   .tbl_fun = ~ .x |>
                     gtsummary::tbl_summary(missing="no",
                                            statistic = list(gtsummary::all_continuous() ~ stat_cont),
                                            by=group,
                                            type= vars[i] ~ "continuous",
                                            digits = list(gtsummary::all_continuous() ~ digits_cont))|>
                     gtsummary::add_n()|>
                     gtsummary::add_overall()|>
                     gtsummary::add_stat(
                       fns = dplyr::everything() ~ add_by_n
                     ) |>
                     gtsummary::modify_header(starts_with("add_n_stat") ~ "**N**") |>
                     gtsummary::modify_table_body(
                       ~ .x |>
                         dplyr::relocate(n, .before = stat_0) |>
                         dplyr::relocate(add_n_stat_1, .before = stat_1) |>
                         dplyr::relocate(add_n_stat_2, .before = stat_2)|>
                         dplyr::relocate(add_n_stat_3, .before = stat_3)
                     )|>
                     gtsummary::modify_header(update = list(label ~ paste0("**", gsub("\\b(\\w)", "\\U\\1", tolower(visit), perl = TRUE),"**"))), quiet = TRUE)
        )
      }
    }

    if (i > 1){
      tbl$table_body <- rbind(tbl$table_body, c(i,1, vars[i], rep(NA, ncol(tbl$table_body)-3)),
                              get(paste0("t", i))$table_body)
    }
    else{
      tbl<-t1
      tbl$table_body<- rbind(c(i,1, vars[i], rep(NA, ncol(tbl$table_body)-3)), t1$table_body)
      }
  }

  # Replace variable names with labels
  for (i in 1:length(vars)){
    tbl[["table_body"]][["label"]] <- as.character(ifelse(tbl[["table_body"]][["label"]]==vars[i], labels[i], tbl[["table_body"]][["label"]]))
  }

  # some edits within the object table_body
  # without grouping variable
  if (is.null(group)){
    tbl$table_body <- tbl$table_body |>
      dplyr::mutate(variable=ifelse(tbl_indent_id1==indent, dplyr::lead(variable), variable),
                    var_type= ifelse(tbl_indent_id1==indent, dplyr::lead(var_type), var_type),
                    row_type= ifelse(tbl_indent_id1==indent, dplyr::lead(row_type), row_type),
                    var_label= ifelse(tbl_indent_id1==indent, dplyr::lead(var_label), var_label),
                    n=ifelse(tbl_indent_id1==indent, dplyr::lead(n), n),
                    stat_0= ifelse(tbl_indent_id1==indent, dplyr::lead(stat_0), stat_0)
      )|>
      dplyr::filter(tbl_indent_id1 !=0)
    if (is.null(visitgroup)){
      tbl[["table_body"]][["tbl_indent_id1"]]<- ifelse(is.na(tbl[["table_body"]][["stat_0"]]), 1, 0)
    }
  }else {
    # if 3 groups
    if (length(unique(data[[group]]))==3){
      tbl$table_body <- tbl$table_body |>
        dplyr::mutate(variable=ifelse(tbl_indent_id1==indent, dplyr::lead(variable), variable),
                      var_type= ifelse(tbl_indent_id1==indent, dplyr::lead(var_type), var_type),
                      row_type= ifelse(tbl_indent_id1==indent, dplyr::lead(row_type), row_type),
                      var_label= ifelse(tbl_indent_id1==indent, dplyr::lead(var_label), var_label),
                      n=ifelse(tbl_indent_id1==indent, dplyr::lead(n), n),
                      add_n_stat_1=ifelse(tbl_indent_id1==indent, dplyr::lead(add_n_stat_1), add_n_stat_1),
                      add_n_stat_2=ifelse(tbl_indent_id1==indent, dplyr::lead(add_n_stat_2), add_n_stat_2),
                      add_n_stat_3=ifelse(tbl_indent_id1==indent, dplyr::lead(add_n_stat_3), add_n_stat_3),
                      stat_0= ifelse(tbl_indent_id1==indent, dplyr::lead(stat_0), stat_0),
                      stat_1= ifelse(tbl_indent_id1==indent, dplyr::lead(stat_1), stat_1),
                      stat_2= ifelse(tbl_indent_id1==indent, dplyr::lead(stat_2), stat_2),
                      stat_3= ifelse(tbl_indent_id1==indent, dplyr::lead(stat_3), stat_3)
        )|>
        dplyr::filter(tbl_indent_id1 !=0)
    } else{# if 2 groups
      tbl$table_body <- tbl$table_body |>
        dplyr::mutate(variable=ifelse(tbl_indent_id1==indent, dplyr::lead(variable), variable),
                      var_type= ifelse(tbl_indent_id1==indent, dplyr::lead(var_type), var_type),
                      row_type= ifelse(tbl_indent_id1==indent, dplyr::lead(row_type), row_type),
                      var_label= ifelse(tbl_indent_id1==indent, dplyr::lead(var_label), var_label),
                      n=ifelse(tbl_indent_id1==indent, dplyr::lead(n), n),
                      add_n_stat_1=ifelse(tbl_indent_id1==indent, dplyr::lead(add_n_stat_1), add_n_stat_1),
                      add_n_stat_2=ifelse(tbl_indent_id1==indent, dplyr::lead(add_n_stat_2), add_n_stat_2),
                      stat_0= ifelse(tbl_indent_id1==indent, dplyr::lead(stat_0), stat_0),
                      stat_1= ifelse(tbl_indent_id1==indent, dplyr::lead(stat_1), stat_1),
                      stat_2= ifelse(tbl_indent_id1==indent, dplyr::lead(stat_2), stat_2)
        )|>
        dplyr::filter(tbl_indent_id1 !=0)
    }
    if (is.null(visitgroup)){
      tbl[["table_body"]][["tbl_indent_id1"]]<- ifelse(is.na(tbl[["table_body"]][["stat_1"]]), 1, 0)
    }
  }
  # if N column not desired
  if (add_n==FALSE){
    if (is.null(group)){
      tbl<-tbl|>
        gtsummary::modify_column_hide(columns = "n")
    }
    else {
      if (length(unique(data[[group]]))==2){
        tbl<-tbl|>
          gtsummary::modify_column_hide(columns = c("n", "add_n_stat_1", "add_n_stat_2"))
      }
      if (length(unique(data[[group]]))==3){
        tbl<-tbl|>
          gtsummary::modify_column_hide(columns = c("n", "add_n_stat_1", "add_n_stat_2", "add_n_stat_3"))
      }
    }
  }
  # if overall column not desired
  if (overall==FALSE & !is.null(group)){
    tbl<-tbl|>
      gtsummary::modify_column_hide(columns = c("stat_0", "n"))
  }
  # if flex_table is needed
  if(as_flex_table == TRUE){
    if (border == TRUE){
      FitFlextableToPage(gtsummary::as_flex_table(tbl)|>
                           flextable::border_outer(part = "header")|>
                           flextable::border_outer(part = "body") )
    }
    else{
      FitFlextableToPage(gtsummary::as_flex_table(tbl))
    }
  }
  else{
    tbl
  }
}
