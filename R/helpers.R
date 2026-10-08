
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

