
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
    if (!var %in% names(data)) return(NULL)

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
      return(NULL)
    }
  })

  # Clean up result
  names(labels) <- vars
  labels <- labels[!sapply(labels, is.null)]
  return(labels)

  ## labels should be same length as vars
  ## if labels: should take labels if vars: should take vars
}


#'Get the labels from a dataset TEST - Sami, please check.
#'
#' This function get the labels from a dataset
#'
#' @param data Dataset
#' @param vars Variables of interest
#' @return The labels
#' @keywords internal
get_labels_test <- function(data, vars) {
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





