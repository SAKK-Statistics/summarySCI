#' Creates publication-ready summary tables for continuous data grouped, by visit
#'
#' @param data A data frame or tibble containing the data to be summarized.
#'
#' @param vars Continuous variables to include in the summary table.
#' Need to be specified with quotes, e.g. `"grade"` or `c("grade", "stage")`. Default to
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
#' @param digits_cat Digits for summary statistics of categorical
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


summaryByVisitCategorical<- function(data,
                          vars = NULL,
                          group = NULL,
                          labels = NULL,
                          stat_cat = "n_percent",
                          visit = "visit",
                          order = NULL,
                          visitgroup = NULL,
                          digits_cat=1,
                          missing_percent=TRUE,
                          missing=TRUE,
                          missing_text = "Missing",
                          add_n = FALSE,
                          overall = FALSE,
                          as_flex_table = TRUE,
                          border = TRUE,
                          word_output = FALSE,
                          file_name = paste0("SummaryByVisit_", format(Sys.Date(), "%Y%m%d"), ".docx")){


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
  # define visit order

  if (!is.null(order)){
    data <- data|>
      dplyr::arrange(order)|>
      as.data.frame()
  } else{
    # order visit numbers not lexicographic
    data <- data|>
      dplyr::mutate(group_num = as.numeric(gsub("[^0-9]", "", visit)))|>
      dplyr::arrange(group_num)|>
      as.data.frame()
  }

  data[[visit]] <- factor(data[[visit]], levels = unique(data[[visit]]))

  # remove rows without visit
  data <- data[(!is.na(data[[visit]])),]
  data <- data[(data[[visit]] != ""),]

  # Summary stat for continuous variables
  stat_cat <- format_lookup_cat[[stat_cat]]

  # if vars = NULL, take all the variables (except group if not NULL).
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }

  # implement missing percent
  data_noMissing<-data
  if(missing_percent != FALSE & missing != FALSE){
    for (i in colnames(data|>
                       dplyr::select(all_of(c(vars))))) {

      if (is.factor(data[[i]]) == TRUE | is.character(data[[i]])) {
        data[[i]] <- forcats::fct_na_value_to_level(as.factor(data[[i]]), level = missing_text)
        # data2[[i]] <- forcats::fct_explicit_na(as.factor(data2[[i]]), na_level = missing_text)
        if (!is.null(attr(data[[i]], "label"))) {
          Hmisc::label(data[[i]]) <- attr(data[[i]], "label")
        }
      } else if (all(data[[i]] %in% c(0, 1, NA))) {
        data[[i]] <- forcats::fct_na_value_to_level(factor(data[[i]]), level = missing_text)
        # data2[[i]] <- forcats::fct_explicit_na(factor(data2[[i]]))
        if (!is.null(attr(data[[i]], "label"))) {
          Hmisc::label(data[[i]]) <- attr(data[[i]], "label")
        }
      }
    }
    }

  if (missing==FALSE){
    missing<-"no"
  }
  if (missing==TRUE){
    missing<-"ifany"
  }

  if (any(sapply(data[vars], is.numeric))) {
    stop("'All vars must be categorical'")
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
      # N without missings
      if (!is.null(visitgroup) & !is.null(order)){
        data_noMissing<- data_noMissing[order(data_noMissing[[visitgroup]], data_noMissing[[order]]),]
        data_noMissing$order2 <- match(data_noMissing[[order]], unique(data_noMissing[[order]]))
        n_values<- as.vector(table(data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$order2))
      }
      else{
        n_values<- as.vector(table(data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$visit))
      }

      assign(paste0("t", i), data|>
               dplyr::select(select_vars)|>
               gtsummary::tbl_strata_nested_stack(
                 .x ,
                 strata = strata0,
                 .tbl_fun = ~ .x |>
                   gtsummary::tbl_summary(missing=missing,
                                          statistic = list(gtsummary::all_categorical() ~ stat_cat),
                                          type= vars[i] ~ "categorical",
                                          digits = list(gtsummary::all_categorical() ~ digits_cat))|>
                   gtsummary::add_n(last=TRUE)|>
                   gtsummary::add_overall(last=TRUE)|>
                   gtsummary::modify_table_body(
                     ~ .x |>
                       dplyr::relocate(n, .before = stat_0))|>
                   gtsummary::modify_header(update = list(label ~ paste0("**", gsub("\\b(\\w)", "\\U\\1", tolower(visit), perl = TRUE),"**"))), quiet = TRUE)%>%
        modify_table_body(
          ~ .x %>%
            dplyr::mutate(
              n = {
                tmp <- n
                tmp[!is.na(tmp)] <- n_values
                tmp
              }
            )
        )
      )
    }
    # for 2 groups
    else {
      if (length(unique(data[[group]]))==2){
        if (!is.null(visitgroup) & !is.null(order)){
          data_noMissing <-data_noMissing[(is.na(data_noMissing[group])==FALSE & is.na(data_noMissing[vars[i]])==FALSE),]
          data_noMissing<- data_noMissing[order(data_noMissing[[visitgroup]], data_noMissing[[order]]),]
          data_noMissing$order2 <- match(data_noMissing[[order]], unique(data_noMissing[[order]]))
          n_values<- as.vector(table(data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$order2))
          n_values_gr<- table(cbind( data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),][group], data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$order2))
        }
        else{
          d_noMiss <-data_noMissing[(is.na(data_noMissing[group])==FALSE & is.na(data_noMissing[vars[i]])==FALSE),]
          n_values<- as.vector(table(data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$visit))
          n_values_gr<- table(cbind( data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),][group], data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$visit))
        }

        assign(paste0("t", i), data|>
                 dplyr::select(select_vars, group)|>
                 gtsummary::tbl_strata_nested_stack(
                   .x ,
                   strata = strata0,
                   .tbl_fun = ~ .x |>
                     gtsummary::tbl_summary(missing=missing,
                                            statistic = list(gtsummary::all_categorical() ~ stat_cat),
                                            by=group,
                                            type= vars[i] ~ "categorical",
                                            digits = list(gtsummary::all_categorical() ~ digits_cat))|>
                     gtsummary::add_n(last=TRUE)|>
                     gtsummary::add_overall(last=TRUE)|>
                     gtsummary::add_stat(
                       fns = dplyr::everything() ~ add_by_n
                     ) |>
                     gtsummary::modify_header(starts_with("add_n_stat") ~ "**N**")  |>
                     gtsummary::modify_table_body(
                       ~ .x |>
                         dplyr::relocate(n, .before = stat_0) |>
                         dplyr::relocate(add_n_stat_1, .before = stat_1) |>
                         dplyr::relocate(add_n_stat_2, .before = stat_2)
                     )|>
                     gtsummary::modify_header(update = list(label ~ paste0("**", gsub("\\b(\\w)", "\\U\\1", tolower(visit), perl = TRUE),"**"))), quiet = TRUE)%>%
                 modify_table_body(
                   ~ .x %>%
                     dplyr::mutate(
                       n = {
                         tmp <- n
                         tmp[!is.na(tmp)] <- n_values
                         tmp
                       },
                       add_n_stat_1 = {
                         tmp <- add_n_stat_1
                         tmp[!is.na(tmp)] <- n_values_gr[1,]
                         tmp
                       },
                       add_n_stat_2 = {
                         tmp <- add_n_stat_2
                         tmp[!is.na(tmp)] <- n_values_gr[2,]
                         tmp
                       }
                     )
                 )
        )
      }
      # for 3 groups
      if (!is.null(visitgroup) & !is.null(order)){
        data_noMissing <-data_noMissing[(is.na(data_noMissing[group])==FALSE & is.na(data_noMissing[vars[i]])==FALSE),]
        data_noMissing<- data_noMissing[order(data_noMissing[[visitgroup]], data_noMissing[[order]]),]
        data_noMissing$order2 <- match(data_noMissing[[order]], unique(data_noMissing[[order]]))
        n_values<- as.vector(table(data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$order2))
        n_values_gr<- table(cbind( data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),][group], data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$order2))
      }
      else{
        d_noMiss <-data_noMissing[(is.na(data_noMissing[group])==FALSE & is.na(data_noMissing[vars[i]])==FALSE),]
        n_values<- as.vector(table(data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$visit))
        n_values_gr<- table(cbind( data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),][group], data_noMissing[(is.na(data_noMissing[vars[i]])==FALSE),]$visit))
      }

      if (length(unique(data[[group]]))==3){
        assign(paste0("t", i), data|>
                 dplyr::select(select_vars, group)|>
                 gtsummary::tbl_strata_nested_stack(
                   .x ,
                   strata = strata0,
                   .tbl_fun = ~ .x |>
                     gtsummary::tbl_summary(missing=missing,
                                            statistic = list(gtsummary::all_categorical() ~ stat_cat),
                                            by=group,
                                            type= vars[i] ~ "categorical",
                                            digits = list(gtsummary::all_categorical() ~ digits_cat))|>
                     gtsummary::add_n(last=TRUE)|>
                     gtsummary::add_overall(last=TRUE)|>
                     gtsummary::add_stat(
                       fns = dplyr::everything() ~ add_by_n
                     ) |>
                     gtsummary::modify_header(starts_with("add_n_stat") ~ "**N**")  |>
                     gtsummary::modify_table_body(
                       ~ .x |>
                         dplyr::relocate(n, .before = stat_0) |>
                         dplyr::relocate(add_n_stat_1, .before = stat_1) |>
                         dplyr::relocate(add_n_stat_2, .before = stat_2)|>
                         dplyr::relocate(add_n_stat_3, .before = stat_3)
                     )|>
                     gtsummary::modify_header(update = list(label ~ paste0("**", gsub("\\b(\\w)", "\\U\\1", tolower(visit), perl = TRUE),"**"))), quiet = TRUE)%>%
                 modify_table_body(
                   ~ .x %>%
                     dplyr::mutate(
                       n = {
                         tmp <- n
                         tmp[!is.na(tmp)] <- n_values
                         tmp
                       },
                       add_n_stat_1 = {
                         tmp <- add_n_stat_1
                         tmp[!is.na(tmp)] <- n_values_gr[1,]
                         tmp
                       },
                       add_n_stat_2 = {
                         tmp <- add_n_stat_2
                         tmp[!is.na(tmp)] <- n_values_gr[2,]
                         tmp
                       },
                       add_n_stat_3 = {
                         tmp <- add_n_stat_3
                         tmp[!is.na(tmp)] <- n_values_gr[3,]
                         tmp
                       }
                     )
                 )
        )
      }
    }

    if (i > 1){
      tbl$table_body <- rbind(tbl$table_body, c(i,1, vars[i], rep(NA, ncol(tbl$table_body)-3)),
                              get(paste0("t", i))$table_body)
    }
    if (i <= 1){
      tbl<-t1
      tbl$table_body<- rbind(c(i,1, vars[i], rep(NA, ncol(tbl$table_body)-3)), t1$table_body)
    }
  }

  # Replace variable names with labels
  for (i in 1:length(vars)){
    tbl[["table_body"]][["label"]] <- as.character(ifelse(tbl[["table_body"]][["label"]]==vars[i], labels[i], tbl[["table_body"]][["label"]]))
  }

  ## some edits within the object table_body
  # Move up N to the desired row
  tbl$table_body<-tbl$table_body|>
  mutate(across(everything(),
                ~ if_else(is.na(.), lead(.), .)))


  # delete label row within visit
  if (!is.null(group)){
  tbl$table_body <- tbl$table_body |>
     dplyr::mutate(variable=ifelse(is.na(add_n_stat_1)==TRUE, dplyr::lead(add_n_stat_1), add_n_stat_1),
                   variable=ifelse(is.na(add_n_stat_2)==TRUE, dplyr::lead(add_n_stat_2), add_n_stat_2))
  }

  # remove undesired varibale rows within visits
  tbl$table_body <- tbl$table_body|>
   filter(!(tbl$table_body$tbl_indent_id1 == 0 & !is.na(tbl$table_body$n)))


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

  # Footnote für N
  tbl <- tbl|>
    modify_footnote_header(
      columns  = n,
      footnote = "N without missing values"
    )|>
    modify_table_styling(columns = starts_with("add_n_stat_"), footnote = "N without missing values")

  # if overall column not desired
  if (overall==FALSE & !is.null(group)){
    tbl<-tbl|>
      gtsummary::modify_column_hide(columns = c("stat_0", "n"))
  }


  # post-process removal of empty "missing" rows
  tbl<-tbl|>
    modify_table_body(
      ~ .x |> dplyr::filter(!(label == "Missing" & stat_0 %in% c("0 (0%)", "0 (NA%)", "0.0 (0.0%)", "0.0 (NA%)", "0.00 (0.00%)", "0.00 (NA%)", "0.000 (0.000%)", "0.000 (NA%)", "0.0000 (0.0000%)", "0.0000 (NA%)", "0.00000 (0.00000%)", "0.00000 (NA%)")))
    )

  # if flex_table is needed
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
