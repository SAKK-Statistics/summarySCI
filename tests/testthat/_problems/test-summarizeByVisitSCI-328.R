# Extracted from test-summarizeByVisitSCI.R:328

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
footnotes <- function(expr) {
  mt <- drawn_calls(expr, "C_mtext")
  unlist(lapply(Filter(function(a) identical(a[[9]], "grey30"), mt),
                `[[`, 1))
}

# test -------------------------------------------------------------------------
d <- sbv_fixture()
res <- summarizeByVisitSCI(d, vars = "hb", group = "Arm", visit = "visit",
                             stat_cont = "median_IQR", plot_cont = "boxplot",
                             as_flex_table = FALSE)
sm <- tidySummaryByVisit(res$table)
z  <- boxes_drawn(res$plots$hb)
for (g in 1:2) {
    s <- sm[sm$group == c("Arm A", "Arm B")[g], ]
    s <- s[order(s$visit_id), ]
    expect_within_rounding(z[[g]]$stats[3, ], s$centre, digits = 1)   # median
    expect_within_rounding(z[[g]]$stats[2, ], s$lower, digits = 1)    # Q1
    expect_within_rounding(z[[g]]$stats[4, ], s$upper, digits = 1)    # Q3
  }
ax <- drawn_calls(print(res$plots$hb), "C_axis")
expect_equal(Filter(function(a) a[[1]] == 1, ax)[[1]][[3]], paste("Cycle", 1:9))
