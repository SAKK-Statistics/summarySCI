
# test data for summaryByVisit
data<-NULL
visit <- c(paste0(rep("Visit ", 10), rbind(c(10:1))),
           paste0(rep("Visit ", 10), rbind(c(10:1))),
           paste0(rep("Visit ", 10), rbind(c(10:1))))
data <- as.data.frame(cbind( visit, rnorm(30)))
data<-as.data.frame(rbind(data, data, data, data, data))
data$visitgroup<- ifelse(data$visit %in% c("Visit 1", "Visit 2"), "Baseline", ifelse(data$visit %in% c("Visit 3", "Visit 4"), "Treatment", "Follow-up"))
data$visitgroup<-factor(data$visitgroup, levels = c("Baseline", "Treatment", "Follow-up"))
data$LDH<-rnorm(150)
data$LDH[5]<-NA
data$Lymphocytes<-rnorm(150)
data$ANC<-rnorm(150)
attr(data$LDH, "label")<-"LLDDHH"
attr(data$Lymphocytes, "label")<-"LLymphoctyes"
attr(data$ANC, "label")<-"AANC"
data$arm<- c(rep("Arm A", 70), rep("Arm B", 80))
data$arm<- c(rep("Arm A", 50), rep("Arm B", 50), rep("Arm C", 50))


test_that("Error when no data given", {
  expect_error(summaryByVisit(data = NULL))
})



test_that("median and range are correct", {
  tbl <- summaryByVisit(data = data,
                      vars = "LDH",
                      as_flex_table = FALSE,
                      digits_cont = 0)

  summary_ldh <- summary(data$LDH[data$visit=="Visit 1"])
  median_1 <- round(summary_ldh["Median"], 0)
  min_1 <- round(summary_ldh["Min."], 0)
  max_1 <- round(summary_ldh["Max."], 0)
  median_range_fct <- tbl[["table_body"]][["stat_0"]][[2]]
  median_range_truth <- paste0(median_1, " (",min_1, ", ", max_1, ")")
  expect_equal(median_range_fct,median_range_truth )

})


test_that("Number of non-missing observations is correct", {
  tbl_5 <- summaryByVisit(data = data,
                        vars = "LDH",
                        visit="visit",
                        as_flex_table = FALSE,
                        add_n = TRUE)

  n_noMissing_fct <- as.numeric(tbl_5[["table_body"]][["n"]][7])
  n_noMissing_truth <- sum(!is.na(data$LDH[visit=="Visit 6"]))
  expect_equal(n_noMissing_fct, n_noMissing_truth)

})

test_that("Number of non-missing observations is correct", {
  tbl_6 <- summaryByVisit(data = data,
                          vars = "LDH",
                          digits_cont = 4,
                          as_flex_table = FALSE,
                          add_n = TRUE)

  summary_ldh <- summary(data$LDH[data$visit=="Visit 1"])
  median_range <- tbl_6[["table_body"]][["stat_0"]][2]
  median_1 <- round(summary_ldh["Median"], 4)
  min_1 <- round(summary_ldh["Min."], 4)
  max_1 <- format(round(summary_ldh["Max."], 4), nsmall = 4)
  median_range_truth <- paste0(median_1, " (",min_1, ", ", max_1, ")")
  expect_equal(median_range, median_range_truth)
})




