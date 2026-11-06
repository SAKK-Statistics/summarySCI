# Extracted from test-boxplotSCI.R:77

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "summarySCI", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
df  <- data.frame(value = c(1, 2, 3, 4, 5, 6, 20))
rec <- capture_boxplotSCI(data = df, var = "value")
expect_equal(box_stats(rec)[[1]][, 1], c(1, 2, 4, 6, 6))
expect_equal(rec$boxplot[[1]]$value$out, 20)
expect_false(isTRUE(all.equal(box_stats(rec)[[1]][2:4, 1],
                                stats::fivenum(df$value)[2:4])))
df$arm <- "A"
tbl <- suppressMessages(summaryTable(df, vars = "value", stat_cont = "median_IQR",
                                       missing = FALSE, as_flex_table = FALSE))
expect_equal(tbl$table_body$stat_0[1], "4.0 (2.0, 6.0)")
