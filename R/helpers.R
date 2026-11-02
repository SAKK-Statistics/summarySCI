
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
