#' Hemoglobin data (to illustrate `boxplot_over_time()`)
#'
#' A dataset containing the hemoglobin value of 169 patients over 9
#' visits. Adapted from SAKK 08/14.
#'
#' @docType data
#'
#' @usage data(hemoglobin_data)
#'
#' @format ## `hemoglobin_data`
#'
#' A data frame with 1307 rows and 5 columns:
#' \describe{
#'   \item{ID}{Patient ID}
#'   \item{Arm}{Experimental arm}
#'   \item{visit}{Visit (factor)}
#'   \item{visit_nr}{Visit (numeric)}
#'   \item{hb}{Hemoglobin value (g/L)}
#' }
#'
#' @source SAKK
"hemoglobin_data"
