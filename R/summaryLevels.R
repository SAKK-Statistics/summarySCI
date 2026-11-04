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
#' Any number of groups (at least two) is possible. Rows with a missing
#' value in `group` are removed. Default to NULL.
#'
#' @param label A label for the new variable to be created.
#' If no label present, the variable name is taken.
#'
#' @param levels = A vector containing the values indicating presence of
#' the factor level. Included by default are "1", "yes", "Yes".
#' All variables in `vars` must be binary (e.g. 0/1, yes/no, TRUE/FALSE),
#' unless `levels` is given; otherwise an error is returned.
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
#' Options include "clopper.pearson" (default), "wilson", "wilson.no.correct",
#' "wald", "wald.no.correct", "agresti.coull" and "jeffreys".
#' If NULL, no CI will be displayed.
#'
#' @param conf_level Numeric. Confidence level. Default to 0.95.
#'
#' @param digits_cat Numeric. Digits for summary statistics and CI of categorical
#' variables. Default to 0.
#'
#' @param overall Logical. If TRUE, an additional column with the total is
#' added at the end of the table. Default to FALSE.
#'
#' @param add_n Logical. If TRUE (default), a column with the number of
#' patients is added before each group column (and before the Overall column).
#' As missing values are treated as absent, N is the number of patients in
#' each group.
#'
#' @param as_flex_table Logical. If TRUE (default) the gtsummary object is
#' converted to a flextable object. Useful when rendering to Word.
#'
#' @param border Logical. If TRUE, a border will be drawn around the table. Only
#' available if flex_table = TRUE. Default is TRUE.
#'
#' @param word_output Logical. If TRUE, the table is also saved in a word document.
#'
#' @param file_path Character string.
#' Specify the path of the Word document containing the table.
#' Only used when `word_output` is TRUE. Needs to end with ".docx".
#'
#' @return A table of class "`flextable`" or `c("tbl_stack", "gtsummary")`.
#' Optionally returns a .docx file in the specified folder.
#'
#' @examples
#' # Site of progression: one column per site, more than one site possible
#' progression <- data.frame(
#'   arm   = rep(c("A", "B"), each = 5),
#'   liver = c("absent", "present", "absent", "present", "absent",
#'             "absent", "present", "absent", "present", "absent"),
#'   lung  = c("absent", "absent", "present", "absent", "absent",
#'             "absent", "absent", "absent", "absent", "absent"),
#'   brain = c("present", "absent", "present", "present", "present",
#'             "present", "present", "present", "present", "present")
#' )
#'
#' summaryLevels(data = progression,
#'               vars = c("liver", "lung", "brain"),
#'               label = "Site of progression",
#'               levels = "present")
#'
#' # By arm, with an overall column
#' summaryLevels(data = progression,
#'               vars = c("liver", "lung", "brain"),
#'               group = "arm",
#'               overall = TRUE,
#'               label = "Site of progression",
#'               levels = "present")
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
                         label = NULL,
                         levels = NULL,
                         stat_cat = "n_percent",
                         test = FALSE,
                         test_cat = "fisher.test",
                         ci = FALSE,
                         ci_cat = "clopper.pearson",
                         conf_level = 0.95,
                         digits_cat = 0,
                         overall = FALSE,
                         add_n = TRUE,
                         as_flex_table = TRUE,
                         border = TRUE,
                         word_output = FALSE,
                         file_path = paste0("SummaryLevels_", format(Sys.Date(), "%Y%m%d"), ".docx")){

  # --------- Some checks --------------------------------------------------- #

    labels <- get_labels(data, vars)


  # Make sure that 'data' exists and that it is a data frame
  if (missing(data)) {
    stop("'data' must be specified.")
  }

  # make sur data is a data frame
  data <- as.data.frame(data)

  # ---- group: a single existing column with at least 2 groups ------------- #
  # Any number of groups is supported. Rows with a missing group are removed
  # (before, they were set to 0 below and shown as an extra group "0").
  if (!is.null(group)) {
    if (!is.character(group) || length(group) != 1) {
      stop("'group' must be a single column name in quotes, e.g. group = \"arm\".",
           call. = FALSE)
    }
    if (!group %in% names(data)) {
      stop("Column '", group, "' (given in 'group') was not found in 'data'.",
           call. = FALSE)
    }
    if (!is.null(vars) && group %in% vars) {
      vars <- setdiff(vars, group)
      message("'", group, "' was removed from 'vars' because it is used as 'group'.")
    }
    n_na_group <- sum(is.na(data[[group]]))
    if (n_na_group > 0) {
      message(n_na_group, " row(s) with missing '", group, "' were removed.")
      data <- data[!is.na(data[[group]]), , drop = FALSE]
    }
    n_groups <- length(unique(data[[group]]))
    if (n_groups < 2) {
      stop("'group' must have at least two groups; '", group, "' has ", n_groups, ".",
           call. = FALSE)
    }
    # group as factor (natural order, as in summaryTable()); the N columns and
    # the group columns then use the same level order (character groups were
    # sorted differently by dplyr and gtsummary, e.g. for mixed upper/lower case)
    if (!is.factor(data[[group]])) {
      data[[group]] <- as_natural_factor(data[[group]])
    } else {
      data[[group]] <- droplevels(data[[group]])
    }
  }

  # NA should be ignored (treated as absent). Thus, replace any NA with 0
  data[is.na(data)] <- 0

  if(!is.null(ci_cat)){

    # Clopper-Pearson is called "exact" in gtsummary ("clopper-pearson" kept
    # for backward compatibility)
    if(ci_cat %in% c("clopper.pearson", "clopper-pearson")){
      ci_cat_gt <- "exact"
    } else if(ci_cat == "wilson" |
              ci_cat == "wilson.no.correct"|
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
  stat_cat <- format_lookup_cat[[stat_cat]]
  # digits per statistic: 0 for counts, digits_cat for percentages
  digits_cat_vec <- cat_digits(stat_cat, digits_cat)
  # CI limits in %, with exactly digits_cat decimals, like the n (%) cells
  # (style_percent() would add one decimal below 10%)
  ci_style <- purrr::partial(gtsummary::style_number, digits = digits_cat, scale = 100)

  # if vars = NULL, take all the variables (except group if not NULL).
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }

  # if levels need to be recoded -------------------------------------------- #
  if (!is.null(levels)){
  for (i in 1:length(vars)){
    data[[vars[i]]] <- as.numeric(as.character(data[[vars[i]]]) %in% as.character(levels))
    }
  }

  # all variables must be binary (one column per possible entry) ------------ #
  is_binary_var <- function(x) {
    v <- unique(tolower(trimws(as.character(x))))
    all(v[!is.na(v)] %in% c("0", "1", "no", "yes", "false", "true"))
  }
  not_binary <- vars[!vapply(data[vars], is_binary_var, logical(1))]
  if (length(not_binary) > 0) {
    show_values <- vapply(not_binary, function(v) {
      vals <- natural_levels(as.character(data[[v]]))
      paste0("`", v, "` (values: ",
             paste(vals[seq_len(min(5, length(vals)))], collapse = ", "),
             if (length(vals) > 5) ", ..." else "", ")")
    }, character(1))
    stop("summaryLevels() needs binary variables (e.g. 0/1 or yes/no), ",
         "one per possible entry. Not binary: ",
         paste(show_values, collapse = "; "), ". ",
         "Use `levels` to define which value(s) count as present, ",
         "or summaryTable() for categorical variables.",
         call. = FALSE)
  }

  # recode to 0/1, so that "yes"/"no" (with NA set to 0), "Yes"/"yes",
  # TRUE/FALSE are all handled the same way
  for (v in vars) {
    data[[v]] <- as.numeric(tolower(trimws(as.character(data[[v]]))) %in% c("1", "yes", "true"))
  }

  # create tables for different combinations -------------------------------- #
  tbl<-NULL

  # N columns: one per group before each group column, and the total N
  # before the Overall column (or before the only column without group)
  add_n_cols <- function(tbl) {
    if (is.null(group)) {
      return(gtsummary::add_n(tbl))
    }
    stat_cols <- grep("^stat_[1-9][0-9]*$", names(tbl$table_body), value = TRUE)
    tbl <- tbl |>
      gtsummary::add_stat(fns = gtsummary::everything() ~ add_by_n) |>
      gtsummary::modify_header(gtsummary::starts_with("add_n_stat") ~ "**N**") |>
      gtsummary::modify_column_alignment(columns = gtsummary::starts_with("add_n_stat"),
                                         align = "center") |>
      gtsummary::modify_table_body(~ purrr::reduce(
        sub("stat_", "", stat_cols, fixed = TRUE),
        .init = .x,
        .f = ~ dplyr::relocate(.x, dplyr::any_of(paste0("add_n_stat_", .y)),
                               .before = dplyr::all_of(paste0("stat_", .y)))
      ))
    if (isTRUE(overall)) {
      tbl <- tbl |>
        gtsummary::add_n() |>
        gtsummary::modify_table_body(
          ~ dplyr::relocate(.x, dplyr::any_of("n"), .before = dplyr::any_of("stat_0")))
    }
    tbl
  }

      for (i in 1:length(vars)){
        if (is.null(group)){
          assign(paste0("t", i), data|>
                   dplyr::select(any_of(c(vars[i])))|>
                   gtsummary::tbl_summary(missing="no",
                                          statistic = list(gtsummary::all_categorical() ~ stat_cat),
                                          label = labels,
                                          digits = list(gtsummary::all_categorical() ~ digits_cat_vec)))
        }
        if (!is.null(group)){
          if (overall==FALSE & test==FALSE){
            assign(paste0("t", i), data|>
                 dplyr::select(any_of(c(vars[i], group)))|>
                 gtsummary::tbl_summary(by= paste0(group), missing="no",
                                        statistic = list(gtsummary::all_categorical() ~ stat_cat),
                                        label = labels,
                                        digits = list(gtsummary::all_categorical() ~ digits_cat_vec)))
          }
          if (overall==TRUE & test==FALSE){
            assign(paste0("t", i), data|>
                     dplyr::select(any_of(c(vars[i], group)))|>
                     gtsummary::tbl_summary(by= paste0(group), missing="no",
                                            statistic = list(gtsummary::all_categorical() ~ stat_cat),
                                            label = labels,
                                            digits = list(gtsummary::all_categorical() ~ digits_cat_vec))|>
                     gtsummary::add_overall(last = TRUE))
          }
          if (overall==TRUE & test==TRUE){
            assign(paste0("t", i), data|>
                     dplyr::select(any_of(c(vars[i], group)))|>
                     gtsummary::tbl_summary(by= paste0(group), missing="no",
                                            statistic = list(gtsummary::all_categorical() ~ stat_cat),
                                            label = labels,
                                            digits = list(gtsummary::all_categorical() ~ digits_cat_vec))|>
                     gtsummary::add_overall(last = TRUE)|>
                     gtsummary::add_p(pvalue_fun = gtsummary::label_style_pvalue(digits = 2),
                           test = list((gtsummary::all_categorical() ~ test_cat)))|>
                     # p-value before the Overall column, as in summaryTable()
                     gtsummary::modify_table_body(
                       ~ dplyr::relocate(.x, dplyr::any_of("p.value"),
                                         .before = dplyr::any_of("stat_0"))))
          }
          if (overall==FALSE & test==TRUE){
            assign(paste0("t", i), data|>
                     dplyr::select(any_of(c(vars[i], group)))|>
                     gtsummary::tbl_summary(by= paste0(group), missing="no",
                                            statistic = list(gtsummary::all_categorical() ~ stat_cat),
                                            label = labels,
                                            digits = list(gtsummary::all_categorical() ~ digits_cat_vec))|>
                     gtsummary::add_p(pvalue_fun = gtsummary::label_style_pvalue(digits = 2),
                           test = list((gtsummary::all_categorical() ~ test_cat))))
          }
        }
        # N columns (as in summaryTable()) --------------------------------- #
        if (isTRUE(add_n)) {
          assign(paste0("t", i), add_n_cols(get(paste0("t", i))))
        }
      if (i > 1){
        if (ci==FALSE){
          tbl <- gtsummary::tbl_stack(list(tbl, get(paste0("t", i))))
        }
        if (ci==TRUE){
         assign(paste0("x", i), get(paste0("t", i))|>
            gtsummary::add_ci(method = list(gtsummary::all_categorical() ~ ci_cat_gt),
                   conf.level = conf_level,
                   style_fun = list(gtsummary::all_categorical() ~ ci_style),
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
                     style_fun = list(gtsummary::all_categorical() ~ ci_style),
                     statistic = list(gtsummary::all_categorical() ~ "[{conf.low}%, {conf.high}%]"))
          }
        }
      }
  # ---------------------------- Fix issues if only 0 ----------------------- #
  # empty cells (variable never present) are shown as 0, for any number of groups
  for (sc in grep("^stat_[0-9]+$", colnames(tbl$table_body), value = TRUE)) {
    tbl$table_body[[sc]] <- ifelse(is.na(tbl$table_body[[sc]]), "0", tbl$table_body[[sc]])
  }
  tbl$table_body <- tbl$table_body[(tbl$table_body$row_type=="label"),]
  # ---------------------------- final column order ------------------------- #
  # Set once at the end, after N, p-value and CI columns exist (add_ci() is
  # applied after add_p() and would otherwise move the p-value to the end):
  # per group N, n (%), CI; then p-value; then N, Overall, CI (as in summaryTable)
  tbl <- gtsummary::modify_table_body(tbl, function(tb) {
    cols <- names(tb)
    grp  <- grep("^stat_[1-9][0-9]*$", cols, value = TRUE)
    grp  <- grp[base::order(as.integer(sub("stat_", "", grp, fixed = TRUE)))]
    k    <- sub("stat_", "", grp, fixed = TRUE)
    per_group <- as.vector(rbind(paste0("add_n_stat_", k), grp, paste0("ci_stat_", k)))
    wanted <- intersect(c(per_group, "p.value", "n", "stat_0", "ci_stat_0"), cols)
    dplyr::relocate(tb, dplyr::all_of(wanted), .after = dplyr::last_col())
  })
  # ---------------------------- add label if any --------------------------- #
  if (!is.null(label)){
    tbl<-tbl|>
      gtsummary::modify_header(!!!list(label ~ paste0("**", label, "**")))|>
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
  # ---------------------------- footnote for N ------------------------------ #
  if (isTRUE(add_n)) {
    tbl <- tbl |>
      gtsummary::modify_table_styling(
        columns = c(dplyr::starts_with("add_n_stat_"), dplyr::any_of("n")),
        footnote = "N = number of patients in the group (missing values counted as absent)"
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
    print(doc, target = file_path)

    message("Table saved to: ", normalizePath(file_path))
  }

  tbl_print

}
