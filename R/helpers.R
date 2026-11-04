
#'Get the labels from a dataset
#'
#' This function get the labels from a dataset
#'
#' @param data Dataset
#' @param vars Variables of interest
#' @return The labels
#' @keywords internal

get_labels <- function(data, vars) {

  labels <- lapply(vars, function(var) {

    # Ensure column exists

    if (!var %in% names(data)) return(var)

    # Use tryCatch in case attr access throws errors on weird types

    lbl <- tryCatch({

      attr(data[[var]], "label")

    }, error = function(e) NULL)

    # If still NULL, attempt labelled::var_label if available

    if (is.null(lbl) && requireNamespace("labelled", quietly = TRUE)) {

      lbl <- labelled::var_label(data[[var]])

      if (is.list(lbl)) lbl <- unlist(lbl)  # var_label returns a named list

    }

    # Final check

    if (!is.null(lbl) && is.character(lbl) && nzchar(lbl)) {

      return(lbl)

    } else {

      return(var)

    }

  })

  names(labels) <- vars

  return(labels)

}



#'Geometric mean
#'
#' This function return the geometric mean
#'
#' @param x Numeric vector
#' @return Geometric mean
#' @keywords internal

geom_mean <- function(x, na.rm = TRUE) {
  exp(mean(log(x), na.rm = na.rm))
}



#' Standard error
#'
#' This function return the standard error
#'
#' @param x Numeric vector
#' @return standard error
#' @keywords internal
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

#' Digits for the categorical statistics
#'
#' Counts (n, N) are shown without decimals; `digits_cat` is used for the
#' percentages only. The digits are returned in the order of the statistics
#' in `stat_fmt`, as gtsummary expects.
#'
#' @param stat_fmt Statistic string, e.g. `"{n} ({p}%)"`.
#' @param digits_cat Digits for percentages.
#' @return Integer vector, one value per statistic.
#' @keywords internal
cat_digits <- function(stat_fmt, digits_cat) {
  stats <- regmatches(stat_fmt, gregexpr("\\{[A-Za-z_.]+\\}", stat_fmt))[[1]]
  ifelse(stats %in% c("{p}", "{p_miss}", "{p_nonmiss}"), digits_cat, 0)
}



add_by_n <- function(data, variable, by, ...) {
    data |>
    dplyr::select(all_of(c(variable, by))) |>
    dplyr::arrange(pick(all_of(c(by, variable)))) |>
    dplyr::group_by(.data[[by]]) |>
    dplyr::summarise(across(everything(), ~sum(!is.na(.)))) |>
    rlang::set_names(c("by", "variable")) |>
    dplyr::mutate(
      by_col = paste0("add_n_stat_", dplyr::row_number()),
      variable = gtsummary::style_number(variable)
    ) %>%
    select(-by) %>%
    tidyr::pivot_wider(names_from = by_col,
                       values_from = variable)
}

add_by_n_by_visit <- function(data, variable, by, ...) {
  data |>
    dplyr::select(all_of(c(variable, by))) |>
    dplyr::arrange(pick(all_of(c(by, variable)))) |>
    dplyr::group_by(.data[[by]], .drop = FALSE) |>          # <- keep empty levels
    dplyr::summarise(across(everything(), ~sum(!is.na(.))), .groups = "drop") |>
    rlang::set_names(c("by", "variable")) |>
    dplyr::mutate(
      by_col = paste0("add_n_stat_", dplyr::row_number()),
      variable = gtsummary::style_number(variable)
    ) %>%
    select(-by) %>%
    tidyr::pivot_wider(names_from = by_col,
                       values_from = variable)
}


FitFlextableToPage <- function(ft, pgwidth = 6){
  ft_out <- ft |> flextable::autofit()
  ft_out <- flextable::width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable::flextable_dim(ft_out)$widths))
  return(ft_out)
}


# needed to know if a variable is dichotomous

is_dichotomous <- function(x) {
  if (is.logical(x)) {
    return(length(unique(na.omit(x))) == 2)
  }

  if (is.numeric(x)) {
    values <- sort(unique(na.omit(x)))
    return(length(values) == 2 && all(values == c(0, 1)))
  }

  if (is.character(x) || is.factor(x)) {
    values <- tolower(trimws(as.character(x)))
    values <- unique(values[!is.na(values) & values != "" & values != "missing"])
    return(length(values) == 2 && setequal(values, c("yes", "no")))
  }

  FALSE
}


#' Natural ordering of levels for character variables
#'
#' Values starting with a number are sorted numerically by that number
#' (e.g. "3" < "10", "2 doses" < "10 doses") and come first. A leading
#' comparison operator is taken into account: "<10%" comes just before "10%",
#' ">90%" just after "90%" (also "<=", ">=", and the symbols for these). All other values
#' follow alphabetically, with embedded numbers compared numerically
#' ("Arm 2" < "Arm 10"). NA is not a level.
#'
#' @param x A character vector.
#' @return Character vector of levels in natural order.
#' @keywords internal
natural_levels <- function(x) {
  u <- unique(as.character(x[!is.na(x)]))
  u_trim <- trimws(u)

  # optional leading comparison operator: <, <=, >, >= and the symbols for
  # "less/greater than or equal to" (created here, so that the source file
  # contains no non-ASCII strings; avoids encoding warnings when parsing)
  le <- intToUtf8(0x2264)
  ge <- intToUtf8(0x2265)
  op_pattern <- paste0("^(<=|>=|<|>|", le, "|", ge, ")")
  op <- regmatches(u_trim, regexpr(op_pattern, u_trim))
  op_full <- rep("", length(u))
  op_full[grepl(op_pattern, u_trim)] <- op
  # "<" just before the number, "<=" next, then the number, ">=", ">"
  op_rank <- stats::setNames(c(-2, -1, -1, 1, 1, 2), c("<", "<=", le, ">=", ge, ">"))
  rank <- ifelse(op_full == "", 0, op_rank[op_full])
  rest <- sub(paste0(op_pattern, "\\s*"), "", u_trim)

  pattern <- "^-?[0-9]+([.,][0-9]+)?"
  has_num <- grepl(pattern, rest)
  num <- rep(NA_real_, length(u))
  lead <- regmatches(rest, regexpr(pattern, rest))
  num[has_num] <- as.numeric(sub(",", ".", lead, fixed = TRUE))
  rank[!has_num] <- 0
  # key with zero-padded digit runs, so that "Arm 2" < "Arm 10"
  key <- u
  digit_runs <- gregexpr("[0-9]+", key)
  regmatches(key, digit_runs) <- lapply(regmatches(key, digit_runs), function(d) {
    formatC(d, width = 20, flag = "0")
  })
  u[order(!has_num, num, rank, key, u)]
}

#' Convert a character vector to a factor with natural level order
#'
#' @param x A character vector.
#' @return A factor; the "label" attribute is kept.
#' @keywords internal
as_natural_factor <- function(x) {
  lab <- attr(x, "label")
  x <- factor(x, levels = natural_levels(x))
  if (!is.null(lab)) attr(x, "label") <- lab
  x
}


#' Robust Fisher's exact test for gtsummary::add_p()
#'
#' Runs stats::fisher.test(); if it fails because the FEXACT workspace is too
#' small (tables with many levels), retries with a larger workspace and finally
#' falls back to a Monte Carlo p-value with a fixed seed (the user's RNG state
#' is restored afterwards). Returns NA if the table is degenerate.
#'
#' @param data,variable,by Passed by gtsummary::add_p().
#' @return A data frame with columns `p.value` and `method`.
#' @keywords internal
fisher_test_safe <- function(data, variable, by, ...) {
  method <- "Fisher's exact test"

  x <- droplevels(as.factor(data[[variable]]))
  g <- droplevels(as.factor(data[[by]]))
  tab <- table(x, g)
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]

  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(data.frame(p.value = NA_real_, method = method))
  }

  # 1. default workspace, 2. larger workspace
  res <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
  if (is.null(res)) {
    res <- tryCatch(stats::fisher.test(tab, workspace = 2e7), error = function(e) NULL)
  }
  if (!is.null(res)) {
    return(data.frame(p.value = res$p.value, method = method))
  }

  # 3. Monte Carlo p-value, reproducible, without touching the user's RNG state
  B <- 1e5
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv()), add = TRUE)
  }
  set.seed(42)
  res <- stats::fisher.test(tab, simulate.p.value = TRUE, B = B)

  data.frame(
    p.value = res$p.value,
    method = sprintf("Fisher's exact test (Monte Carlo p-value, B = %s)",
                     format(B, big.mark = ",", scientific = FALSE))
  )
}



# Function to detect dates variables

# Internal: stop if any variable is a date / date-time
check_no_dates <- function(data, vars, group = NULL) {
  is_date <- function(x) inherits(x, c("Date", "POSIXt", "difftime"))
  all_v    <- unique(c(vars, group))
  date_v   <- all_v[vapply(data[all_v], is_date, logical(1))]

  if (length(date_v) > 0) {
    stop(sprintf(
      "summaryTable() is not adapted for date variables. Remove %s from `vars`%s, or convert %s first (e.g. to a duration with difftime() or to a year with format(x, \"%%Y\")).",
      paste0("`", date_v, "`", collapse = ", "),
      if (!is.null(group) && group %in% date_v) " / `group`" else "",
      if (length(date_v) == 1) "it" else "them"
    ), call. = FALSE)
  }
  invisible(TRUE)
}




#' Resolve a column-name argument
#'
#' Turns the argument of a plotting function into column name(s). Accepts
#' quoted names (`"x"`, `c("x", "y")`), a variable holding such names, and
#' bare names (`x`): a bare name does not evaluate to a character vector, so
#' it is deparsed instead.
#'
#' @param sub The unevaluated argument, i.e. `substitute(arg)`.
#' @param val_fun A function evaluating the argument, i.e. `function() arg`.
#' @return A character vector, or `NULL` if the argument is `NULL`.
#' @keywords internal
col_names <- function(sub, val_fun) {
  if (is.null(sub)) return(NULL)
  val <- tryCatch(val_fun(), error = function(e) NULL)
  if (is.character(val)) val else deparse(sub)
}



#' SCI color palette
#'
#' The default colors of all plotting functions of the package.
#'
#' @param n Number of colors.
#' @param lines Logical. If `TRUE`, only the darker shades are used, since the
#' light ones are hard to see as lines and points on a white background.
#' Default is `FALSE`, for filled boxes and bars.
#' @return A character vector of `n` hex colors, light to dark.
#' @keywords internal
sci_palette <- function(n, lines = FALSE) {
  base <- c("#FFE3E3", "#F4B9C2", "#E4002B", "#961D13", "#401F28")
  if (lines) {
    dark <- base[3:5]
    return(switch(as.character(n),
                  "1" = dark[1],
                  "2" = dark[c(1, 3)],
                  "3" = dark,
                  grDevices::colorRampPalette(dark)(n)))
  }
  switch(as.character(n),
         "1" = base[3],
         "2" = base[2:3],
         "3" = base[1:3],
         "4" = base[1:4],
         "5" = base,
         grDevices::colorRampPalette(base)(n))
}



#' Convert a gtsummary table to a flextable
#'
#' The conversion [summaryTable()] applies when `as_flex_table = TRUE`: a
#' flextable fitted to the page width, with an outer border if requested.
#'
#' @param tbl A gtsummary table.
#' @param border Logical. Draw a border around header and body.
#' @return A flextable.
#' @keywords internal
to_flextable <- function(tbl, border = TRUE) {
  ft <- gtsummary::as_flex_table(tbl)
  if (isTRUE(border)) {
    ft <- ft |>
      flextable::border_outer(part = "header") |>
      flextable::border_outer(part = "body")
  }
  FitFlextableToPage(ft)
}



#' Layout of the plots
#'
#' Margins, lines of the patient numbers and of the x-axis title shared by
#' [boxplotSCI()], [lineplotSCI()] and [stackedbarplotSCI()], so that plots used
#' together line up. The blueprint is the layout of [boxplotSCI()].
#'
#' @param n_time Number of time points; `1` for a single time point.
#' @param n_groups Number of groups.
#' @param risk_table Logical. Are patient numbers drawn beneath the x-axis?
#' @param offset Lines taken by extra labels beneath the x-axis (e.g. group
#' labels under bars, or rotated labels), by which the numbers and the x-axis
#' title move down.
#' @return A list with `mar` (for `par(mar = )`), `rows` (margin lines of the
#' rows of patient numbers, one per group over time, one for a single time
#' point) and `xlab_line`.
#' @keywords internal
sci_layout <- function(n_time, n_groups, risk_table, offset = 0) {
  if (n_time == 1L) {
    bottom    <- if (risk_table) 4.6 else 4.0
    left      <- 4.5
    rows      <- 2
    xlab_line <- if (risk_table) 3.6 else 3.0
  } else {
    base      <- if (n_time > 8) 6.5 else 5.0
    bottom    <- if (risk_table) base + (n_groups - 1) * 0.65 else base - 1
    left      <- if (n_time > 8) 5.5 else 4.5
    rows      <- 2 + (seq_len(n_groups) - 1) * 0.65
    xlab_line <- if (risk_table) 2 + (n_groups - 1) * 0.65 + 1.2 else 3.0
  }
  list(mar = c(bottom + offset, left, 4, 2) + 0.1,
       rows = rows + offset, xlab_line = xlab_line + offset)
}



#' One row of patient numbers beneath the x-axis
#'
#' Draws the numbers `n` at the x positions `at` on margin line `line`, zero
#' counts shown as `missing_nr`, and optionally the row label `"# <label>"`
#' left of the plot, as in [boxplotSCI()].
#'
#' @param n Numbers of patients.
#' @param line Margin line of the row.
#' @param at x positions of the numbers.
#' @param label Group label of the row, or `NULL` for none.
#' @param label_at x position of the right end of the label.
#' @param cex Font size.
#' @param missing_nr Shown instead of zero counts.
#' @keywords internal
risk_row <- function(n, line, at, label = NULL, label_at = NULL, cex = 0.7,
                     missing_nr = 0) {
  n <- as.vector(n)
  n[n == 0] <- missing_nr
  mtext(text = n, side = 1, line = line, at = at, cex = cex)
  if (!is.null(label))
    mtext(text = paste("#", label), side = 1, line = line, at = label_at,
          cex = cex, adj = 1)
}
