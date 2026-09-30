# ============================================================================ #
#  Early input checks for summaryByVisit()
#
#  Put this file in summarySCI/R/ and call the checker at the very top of
#  summaryByVisit(), BEFORE any computation:
#
#    summaryByVisit <- function(data, vars = NULL, ..., file_path = NULL) {
#      chk <- check_summaryByVisit_args(
#        data = data, vars = vars, group = group, labels = labels,
#        stat_cont = stat_cont, stat_cat = stat_cat, visit = visit,
#        order = order, visitgroup = visitgroup,
#        digits_cont = digits_cont, digits_cat = digits_cat,
#        missing_percent = missing_percent, missing = missing,
#        missing_text = missing_text, add_n = add_n, overall = overall,
#        continuous_as_categorical = continuous_as_categorical,
#        as_flex_table = as_flex_table, border = border,
#        word_output = word_output,
#        file_path = if (missing(file_path)) NULL else file_path)
#      vars   <- chk$vars     # resolved (default = all columns except group/visit/...)
#      labels <- chk$labels   # NULL or labels in the same order as vars
#      ...
#    }
#
#  Recommended: change the default `file_path = file_path` to `file_path = NULL`.
#  The current default refers to itself and gives "promise already under
#  evaluation" when word_output = TRUE and no file_path is given.
#
#  Each check below lists the tests from test_summaryByVisit.R it addresses.
# ============================================================================ #

#' Check the arguments of summaryByVisit() and stop early with a clear message
#'
#' @return Invisibly, a list with the resolved `vars` and `labels`.
#' @noRd
check_summaryByVisit_args <- function(data, vars, group, labels,
                                      stat_cont, stat_cat,
                                      visit, order, visitgroup,
                                      digits_cont, digits_cat,
                                      missing_percent, missing, missing_text,
                                      add_n, overall, continuous_as_categorical,
                                      as_flex_table, border,
                                      word_output, file_path) {

  # ---- small helpers -------------------------------------------------------- #
  abort     <- function(...) stop(paste0(...), call. = FALSE)
  is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  is_flag   <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
  is_digits <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) &&
    x >= 0 && x == round(x)
  q         <- function(x) paste0('"', x, '"', collapse = ", ")
  check_col <- function(x, arg) {
    if (!is_string(x))
      abort("`", arg, "` must be a single column name in quotes, e.g. `",
            arg, ' = "name"`.')
    if (!x %in% names(data))
      abort("`", arg, "` = ", q(x), " is not a column of `data`.")
    invisible(TRUE)
  }

  # ---- data ------------------------------------------------------------------ #
  # L01, L05
  if (!is.data.frame(data))
    abort("`data` must be a data frame or tibble, not ", class(data)[1], ".")
  if (nrow(data) == 0L)
    abort("`data` has 0 rows.")

  # ---- visit ----------------------------------------------------------------- #
  # F12-F15
  check_col(visit, "visit")

  # ---- group ----------------------------------------------------------------- #
  if (!is.null(group)) {
    check_col(group, "group")                                        # H10, H11
    # F16
    if (identical(group, visit))
      abort("`group` and `visit` must be different columns.")
    g <- data[[group]]
    # H03, H07, H08, H14, R-cases with arm_na:
    # NA in group was counted as an extra group ("add_n_stat_3 not found",
    # "maximum of 3 groups" for a 3-level group with NA)
    # if (anyNA(g))
    #   abort("`group` = ", q(group), " contains ", sum(is.na(g)),
    #         " missing value(s). Remove these rows or recode the missing ",
    #         "values (e.g. as \"Unknown\") before calling summaryByVisit().")
    n_grp <- length(unique(g))
    # H04, L07 ("object 't1' not found")
    if (n_grp < 2L)
      abort("`group` = ", q(group), " has only ", n_grp,
            " group. At least 2 groups are needed; use `group = NULL` ",
            "for an ungrouped table.")
    # H02, H09, H13
    if (n_grp > 3L)
      abort("`group` = ", q(group), " has ", n_grp,
            " groups. A maximum of 3 groups is currently supported.")
  }

  # ---- order ----------------------------------------------------------------- #
  if (!is.null(order)) {
    check_col(order, "order")                                        # F24
    o <- data[[order]]
    # F22
    if (!is.numeric(o))
      abort("`order` = ", q(order), " must be numeric, not ", class(o)[1], ".")
    # F23: every visit must have exactly one order value and vice versa
    vo <- unique(data.frame(v = data[[visit]], o = o)[!is.na(data[[visit]]) & !is.na(o), ])
    if (anyDuplicated(vo$v) || anyDuplicated(vo$o))
      abort("`order` = ", q(order), " must have exactly one value per visit ",
            "(each visit one order number, each order number one visit).")
  }

  # ---- visitgroup ------------------------------------------------------------ #
  if (!is.null(visitgroup)) {
    check_col(visitgroup, "visitgroup")                              # F40
    vg <- data[[visitgroup]]
    # F34, F35
    if (!is.ordered(vg))
      # abort("`visitgroup` = ", q(visitgroup), " must be an ordered factor, not ",
      #       if (is.factor(vg)) "an unordered factor" else class(vg)[1],
      #       ". Use factor(x, levels = ..., ordered = TRUE).")
    if (!is.null(group) && identical(visitgroup, group))
      abort("`visitgroup` and `group` must be different columns.")
    # F38: each visit must belong to exactly one visit group
    vv <- unique(data.frame(v = data[[visit]], g = vg)[!is.na(data[[visit]]) & !is.na(vg), ])
    if (anyDuplicated(vv$v))
      abort("`visitgroup` = ", q(visitgroup), " must be constant within each visit. ",
            "Visits in more than one visit group: ",
            paste(head(unique(vv$v[duplicated(vv$v)]), 5), collapse = ", "), ".")
  }

  # ---- vars ------------------------------------------------------------------ #
  # NOTE: keep this default identical to the one used in summaryByVisit()
  if (is.null(vars))
    vars <- setdiff(names(data), c(group, visit, order, visitgroup))
  # L04, L09
  if (!is.character(vars) || length(vars) == 0L || anyNA(vars))
    abort("`vars` must be a character vector of column names, e.g. ",
          '`vars = c("age", "sex")`.')
  # L02, L03
  not_in <- setdiff(vars, names(data))
  if (length(not_in))
    abort("`vars` contains column(s) not in `data`: ", q(not_in), ".")
  # L10
  if (anyDuplicated(vars))
    abort("`vars` contains duplicates: ", q(unique(vars[duplicated(vars)])), ".")
  # A11
  if (!is.null(group) && group %in% vars)
    abort("`group` = ", q(group), " cannot also be in `vars`.")
  # A12 (previously the misleading message "All vars must be numeric")
  if (visit %in% vars)
    abort("`visit` = ", q(visit), " cannot also be in `vars`.")
  if (!is.null(order) && order %in% vars)
    abort("`order` = ", q(order), " cannot also be in `vars`.")
  # F42
  if (!is.null(visitgroup) && visitgroup %in% vars)
    abort("`visitgroup` = ", q(visitgroup), " cannot also be in `vars`.")

  # ---- labels ---------------------------------------------------------------- #
  # B02, B03, B04, B07, M01, M02, R-cases with labels
  # ("subscript out of bounds" / "attempt to replicate an object of type 'language'")
  if (!is.null(labels)) {
    if (!(is.character(labels) || is.list(labels)))
      abort("`labels` must be a character vector or a list, e.g. ",
            '`labels = c("Age (years)", "Sex")`.')
    if (is.list(labels) && any(vapply(labels, inherits, logical(1), "formula")))
      abort("`labels` must not use formula syntax (age ~ \"Age\"). ",
            'Use `labels = c(age = "Age", ...)` instead.')
    ok <- vapply(labels, function(l) is.character(l) && length(l) == 1L && !is.na(l),
                 logical(1))
    if (!all(ok))
      abort("Each element of `labels` must be a single, non-missing character string.")
    if (length(labels) != length(vars))
      abort("`labels` must have the same length as `vars` (", length(vars),
            "), but has length ", length(labels), ".\n",
            "  vars: ", q(vars), if (length(vars) > 10) " ..." else "")
    nm <- names(labels)
    if (!is.null(nm) && any(nzchar(nm))) {
      # Named: names must be exactly the vars (any order) -> reorder to vars
      if (!setequal(nm, vars) || anyDuplicated(nm))
        abort("The names of `labels` must match `vars`.\n",
              "  Not in vars: ", q(setdiff(nm, vars)), "\n",
              "  Missing:     ", q(setdiff(vars, nm)))
      labels <- labels[vars]
    } else {
      # Unnamed: taken in the order of vars
      names(labels) <- vars
    }
  }

  # ---- statistics ------------------------------------------------------------ #
  stat_cont_opts <- c("median_IQR", "median_range", "mean_sd", "mean_se", "geomMean_sd")
  stat_cat_opts  <- c("n", "n_N", "n_percent")
  # C92, C94
  if (!is_string(stat_cont) || !stat_cont %in% stat_cont_opts)
    abort("`stat_cont` must be one of ", q(stat_cont_opts), ".")
  # C93
  if (!is_string(stat_cat) || !stat_cat %in% stat_cat_opts)
    abort("`stat_cat` must be one of ", q(stat_cat_opts), ".")

  # ---- digits ---------------------------------------------------------------- #
  # G20, G21, G22, G24
  if (!is_digits(digits_cont))
    abort("`digits_cont` must be a single whole number >= 0, e.g. 1.")
  if (!is_digits(digits_cat))
    abort("`digits_cat` must be a single whole number >= 0, e.g. 1.")

  # ---- missing --------------------------------------------------------------- #
  # E24, L08
  if (!is_flag(missing))
    abort("`missing` must be TRUE or FALSE.")
  # E25
  if (!(is_flag(missing_percent) || identical(missing_percent, "both")))
    abort('`missing_percent` must be TRUE, FALSE or "both".')
  # E22, L21 (empty string "" is allowed)
  if (!(is.character(missing_text) && length(missing_text) == 1L && !is.na(missing_text)))
    abort("`missing_text` must be a single character string, e.g. \"Missing\".")

  # ---- continuous_as_categorical --------------------------------------------- #
  # D26 ("undefined columns selected")
  if (!is.null(continuous_as_categorical)) {
    if (!is.character(continuous_as_categorical) || anyNA(continuous_as_categorical))
      abort("`continuous_as_categorical` must be a character vector of column names.")
    not_in <- setdiff(continuous_as_categorical, names(data))
    if (length(not_in))
      abort("`continuous_as_categorical` contains column(s) not in `data`: ", q(not_in), ".")
  }

  # ---- logical switches ------------------------------------------------------ #
  # L22
  for (a in c("add_n", "overall", "as_flex_table", "border", "word_output")) {
    if (!is_flag(get(a)))
      abort("`", a, "` must be TRUE or FALSE.")
  }

  # ---- Word output ----------------------------------------------------------- #
  # J03, J04, J05, J08
  if (word_output) {
    if (is.null(file_path) || !is_string(file_path))
      abort("`file_path` must be given when `word_output = TRUE`, e.g. ",
            '`file_path = "tables/visit_table.docx"`.')
    if (!grepl("\\.docx$", file_path, ignore.case = TRUE))
      abort("`file_path` must end with \".docx\": ", q(file_path), ".")
    if (!dir.exists(dirname(file_path)))
      abort("The folder of `file_path` does not exist: ", q(dirname(file_path)), ".")
  }

  invisible(list(vars = vars, labels = labels))
}
