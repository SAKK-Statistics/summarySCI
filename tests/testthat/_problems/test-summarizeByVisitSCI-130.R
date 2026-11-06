# Extracted from test-summarizeByVisitSCI.R:130

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "summarySCI", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran()
sbv_fixture <- function() {
  d <- hb_fixture()
  set.seed(7)
  d$score <- sample(c(1, 2, 10), nrow(d), replace = TRUE)   # numeric levels
  d
}
check_lines_match_table <- function(res, var, label, digits, k = 1) {
  sm  <- tidySummaryByVisit(res$table)
  sm  <- sm[sm$variable == label, ]
  num <- res$plots[[var]]$numbers
  # one group: the table calls it "Overall", the plot names it after the variable
  grp <- if (nrow(num$centre) == 1) rep(1L, nrow(sm)) else match(sm$group, rownames(num$centre))
  idx <- cbind(grp, sm$visit_id)
  expect_within_rounding(num$centre[idx], sm$centre, digits)
  expect_within_rounding(num$lower[idx], sm$lower, digits, k)
  expect_within_rounding(num$upper[idx], sm$upper, digits, k)
  expect_equal(as.numeric(num$n[idx]), sm$n)
}
pct_decimals <- function(stat) {
  x <- sub("^.*\\(([0-9.]+)%\\).*$", "\\1", stat)
  ifelse(grepl(".", x, fixed = TRUE), nchar(sub("^[^.]*\\.", "", x)), 0L)
}
check_bars_match_table <- function(res, var, label, digits = NULL,
                                   missing_text = "Missing") {
  sm  <- tidySummaryByVisit(res$table)
  sm  <- sm[sm$variable == label, ]
  num <- res$plots[[var]]$numbers
  for (i in seq_len(nrow(sm))) {
    lev <- sm$level[i]
    if (!lev %in% dimnames(num$counts)[[1]]) {
      # only the table's missing row may be absent from the plot
      expect_equal(lev, missing_text)
      next
    }
    g <- if (dim(num$counts)[2] == 1) 1L else sm$group[i]
    t <- sm$visit_id[i]
    expect_equal(num$counts[lev, g, t], sm$count[i],
                 label = paste(var, lev, g, sm$visit[i]))
    if (grepl("%", sm$stat[i])) {
      printed <- as.numeric(sub("^.*\\(([0-9.]+)%\\).*$", "\\1", sm$stat[i]))
      d <- if (is.null(digits)) pct_decimals(sm$stat[i]) else digits
      expect_within_rounding(num$percent[lev, g, t], printed, d)
    }
  }
}

# test -------------------------------------------------------------------------
d <- sbv_fixture()
d$week <- c(0, 2, 4, 6, 8, 12, 16, 20, 24)[d$visit_nr]
res <- summarizeByVisitSCI(d, vars = c("hb", "hb_cat"), group = "Arm",
                             visit = "visit", order = "week",
                             as_flex_table = FALSE)
expect_equal(colnames(res$plots$hb$numbers$centre), paste("Cycle", 1:9))
ax <- drawn_calls(print(res$plots$hb), "C_axis")
ax <- Filter(function(a) a[[1]] == 1, ax)[[1]]
expect_equal(ax[[2]], c(0, 2, 4, 6, 8, 12, 16, 20, 24))
