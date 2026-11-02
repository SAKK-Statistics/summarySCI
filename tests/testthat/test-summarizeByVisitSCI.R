# The plots of summarizeByVisitSCI() are drawn from the raw data. These tests
# check that they show the numbers printed in the table: the table is read
# back with tidySummaryByVisit(), and every plotted value is compared with it.

sbv_fixture <- function() {
  d <- hb_fixture()
  set.seed(7)
  d$score <- sample(c(1, 2, 10), nrow(d), replace = TRUE)   # numeric levels
  d
}

# Line plot: centre, interval and N of every group x visit equal the table's
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

# Stacked bars: every count and percentage equals the table's
check_bars_match_table <- function(res, var, label, digits = 1,
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
      expect_within_rounding(num$percent[lev, g, t], printed, digits)
    }
  }
}

test_that("type chooses the table, the plots or both", {
  d <- sbv_fixture()
  res <- summarizeByVisitSCI(d, vars = "hb", group = "Arm", visit = "visit",
                             type = "table")
  expect_s3_class(res, c("summarizeByVisitSCI", "summarizeSCI"))
  expect_s3_class(res$table, "flextable")
  expect_length(res$plots, 0)
  res <- summarizeByVisitSCI(d, vars = "hb", group = "Arm", visit = "visit",
                             type = "plot")
  expect_null(res$table)
  expect_equal(res$plots$hb$type, "line plot")
})

test_that("each variable gets the plot of its type in the table", {
  d <- sbv_fixture()
  res <- summarizeByVisitSCI(d, vars = c("hb", "hb_cat"), group = "Arm",
                             visit = "visit", as_flex_table = FALSE)
  expect_equal(vapply(res$plots, function(p) p$type, ""),
               c(hb = "line plot", hb_cat = "stacked barplot"))
  res <- summarizeByVisitSCI(d, vars = "hb", group = "Arm", visit = "visit",
                             plot_cont = "boxplot",
                             as_flex_table = FALSE)
  expect_equal(res$plots$hb$type, "boxplot")
  expect_no_error(draw_quietly(print(res$plots$hb)))
  expect_output(print(res), "hb +boxplot")
})

test_that("the line plot shows the statistic of the table", {
  d <- sbv_fixture()
  for (st in c("median_range", "median_IQR", "mean_sd", "mean_se", "geomMean_sd")) {
    res <- summarizeByVisitSCI(d, vars = "hb", group = "Arm", visit = "visit",
                               stat_cont = st, digits_cont = 2,
                               as_flex_table = FALSE)
    # mean +/- SD or SE: both numbers are rounded in the table
    check_lines_match_table(res, "hb", "Hemoglobin", digits = 2,
                            k = if (st %in% c("mean_sd", "mean_se")) 2 else 1)
  }
})

test_that("the bars show the numbers of the table", {
  d <- sbv_fixture()
  for (mp in list(TRUE, FALSE)) {
    res <- summarizeByVisitSCI(d, vars = "hb_cat", group = "Arm", visit = "visit",
                               missing_percent = mp,
                               as_flex_table = FALSE)
    # missing values form a segment exactly when the table's percentages count them
    expect_equal("Missing" %in% dimnames(res$plots$hb_cat$numbers$counts)[[1]], mp)
    check_bars_match_table(res, "hb_cat", "hb_cat")
  }
  res <- summarizeByVisitSCI(d, vars = "hb_cat", group = "Arm", visit = "visit",
                             missing = FALSE, stat_cat = "n_N",
                             as_flex_table = FALSE)
  expect_false("Missing" %in% dimnames(res$plots$hb_cat$numbers$counts)[[1]])
  check_bars_match_table(res, "hb_cat", "hb_cat")
})

test_that("an order column sets the time axis, in the order of the table", {
  d <- sbv_fixture()
  d$week <- c(0, 2, 4, 6, 8, 12, 16, 20, 24)[d$visit_nr]   # uneven spacing
  res <- summarizeByVisitSCI(d, vars = c("hb", "hb_cat"), group = "Arm",
                             visit = "visit", order = "week",
                             as_flex_table = FALSE)
  expect_equal(colnames(res$plots$hb$numbers$centre),
               as.character(c(0, 2, 4, 6, 8, 12, 16, 20, 24)))
  check_lines_match_table(res, "hb", "Hemoglobin", digits = 1)
  check_bars_match_table(res, "hb_cat", "hb_cat")
})

test_that("visit groups and continuous_as_categorical", {
  d <- sbv_fixture()
  res <- summarizeByVisitSCI(d, vars = c("hb", "score"), group = "Arm",
                             visit = "visit", visitgroup = "vg",
                             continuous_as_categorical = "score",
                             as_flex_table = FALSE)
  expect_equal(res$plots$score$type, "stacked barplot")
  # numeric levels in numeric order, as in the table
  expect_equal(dimnames(res$plots$score$numbers$counts)[[1]][1:3],
               c("1", "2", "10"))
  check_lines_match_table(res, "hb", "Hemoglobin", digits = 1)
  check_bars_match_table(res, "score", "score")
})

test_that("ungrouped summaries plot one line and one bar per visit", {
  d <- sbv_fixture()
  res <- summarizeByVisitSCI(d, vars = c("hb", "hb_cat"), visit = "visit",
                             as_flex_table = FALSE)
  expect_equal(nrow(res$plots$hb$numbers$centre), 1)
  check_lines_match_table(res, "hb", "Hemoglobin", digits = 1)
  check_bars_match_table(res, "hb_cat", "hb_cat")
})

test_that("plot_args reach the plots, and plots are saved", {
  d <- sbv_fixture()
  res <- summarizeByVisitSCI(d, vars = c("hb", "hb_cat"), group = "Arm",
                             visit = "visit", as_flex_table = FALSE,
                             plot_args = list(risk_table = TRUE, dodge = 0.2))
  dir <- file.path(tempdir(), "summarizeByVisitSCI-plots")
  on.exit(unlink(dir, recursive = TRUE))
  files <- save_plots(res, file_path = dir, width = 7, height = 5)
  expect_true(all(file.exists(files)))
  expect_warning(summarizeByVisitSCI(d, vars = "hb", visit = "visit",
                                     plot_args = list(nonsense = 1)),
                 "not used by any plot: nonsense")
})

test_that("informative errors", {
  d <- sbv_fixture()
  expect_error(summarizeByVisitSCI(), "'data' must be specified")
  expect_error(summarizeByVisitSCI(d, vars = "hb", word_output = TRUE),
               "not available")
  expect_error(summarizeByVisitSCI(d, vars = "hb", plot_cont = "violin"),
               "should be one of")
})
