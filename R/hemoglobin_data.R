#' Hemoglobin data (to illustrate `boxplotSCI()` and `summaryByVisit()`)
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
#'   \item{Arm}{Experimental arm ("Arm A", "Arm B")}
#'   \item{visit}{Visit (character, "Cycle 1" to "Cycle 9")}
#'   \item{visit_nr}{Visit number (numeric, 1 to 9)}
#'   \item{hb}{Hemoglobin value (g/L)}
#' }
#'
#' @source SAKK
"hemoglobin_data"
