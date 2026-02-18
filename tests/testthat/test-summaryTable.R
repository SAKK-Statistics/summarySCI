
test_that("Error when no data given", {
  expect_error(summaryTable(data = NULL))
})




test_that("median and range are correct", {
  trial <- gtsummary::trial
  tbl <- summaryTable(data = trial,
                      vars = "age",
                      as_flex_table = FALSE,
                      digits_cont = 0)

  summary_age <- summary(trial$age)
  median_1 <- round(summary_age["Median"], 0)
  min_1 <- round(summary_age["Min."], 0)
  max_1 <- round(summary_age["Max."], 0)

  median_range_fct <- tbl[["table_body"]][["stat_0"]][[1]]
  median_range_truth <- paste0(median_1, " (",min_1, ", ", max_1, ")")
  expect_equal(median_range_fct,median_range_truth )
})




test_that("Number of non-missing observations is correct", {
  trial <- gtsummary::trial

  tbl_5 <- summaryTable(data = trial,
                        vars = "age",
                        as_flex_table = FALSE,
                        add_n = TRUE)

  n_noMissing_fct <- as.numeric(tbl_5[["table_body"]][["N"]][1])
  n_noMissing_truth <- sum(!is.na(trial$age))
  expect_equal(n_noMissing_fct, n_noMissing_truth)
})

test_that("Row missing if missing is true", {
  trial <- gtsummary::trial

tbl6 <- summaryTable(data = trial,
                     vars = "response",
                     group = "grade",
                     as_flex_table = FALSE,
                     add_n = TRUE,
                     overall = TRUE,
                     missing = TRUE)


isMissing <- "Missing" %in% tbl6[["table_body"]][["label"]]


expect_true(isMissing)
})


test_that("N without missing add up to overall N without missing", {
tbl7 <- summaryTable(data = trial,
                     vars = "response",
                     group = "grade",
                     as_flex_table = FALSE,
                     add_n = TRUE,
                     overall = TRUE,
                     missing = TRUE,
                     missing_percent = FALSE)

n1 <- as.numeric(tbl7[["table_body"]][["add_n_stat_1"]])[1]
n2 <-  as.numeric(tbl7[["table_body"]][["add_n_stat_2"]])[1]
n3 <- as.numeric(tbl7[["table_body"]][["add_n_stat_3"]])[1]

n_overall <- as.numeric(tbl7[["table_body"]][["n"]])[1]

expect_equal(sum(n1, n2, n3), n_overall)


})



