# Slow (several minutes): run locally and on GitHub, not on CRAN
skip_on_cran()

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

# Number of decimals of a printed percentage, e.g. "12 (40.0%)" -> 1
pct_decimals <- function(stat) {
  x <- sub("^.*\\(([0-9.]+)%\\).*$", "\\1", stat)
  ifelse(grepl(".", x, fixed = TRUE), nchar(sub("^[^.]*\\.", "", x)), 0L)
}

# Stacked bars: every count and percentage equals the table's
# (digits = NULL: taken from the printed table, so the check follows digits_cat)
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
    # mean +/- SD or SE: both numbers are rounded in the table; geometric
    # mean times / divided by geometric SD: up to (mean + GSD + 1) rounding units
    k <- switch(st, mean_sd = , mean_se = 2,
                geomMean_sd = 3 + max(d$hb, na.rm = TRUE), 1)
    check_lines_match_table(res, "hb", "Hemoglobin", digits = 2, k = k)
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

test_that("word_output saves the table as summaryByVisit() does", {
  d <- sbv_fixture()
  f  <- tempfile(fileext = ".docx")
  f2 <- tempfile(fileext = ".docx")
  on.exit(unlink(c(f, f2)))
  expect_message(res <- summarizeByVisitSCI(d, vars = "hb", group = "Arm",
                                            visit = "visit", word_output = TRUE,
                                            file_path = f),
                 "Table saved to")
  expect_true(file.exists(f))
  expect_length(res$plots, 1)
  suppressMessages(summaryByVisit(d, vars = "hb", group = "Arm", visit = "visit",
                                  word_output = TRUE, file_path = f2))
  doc_text <- function(path) officer::docx_summary(officer::read_docx(path))$text
  expect_equal(doc_text(f), doc_text(f2))
})

test_that("informative errors", {
  d <- sbv_fixture()
  expect_error(summarizeByVisitSCI(), "'data' must be specified")
  expect_error(summarizeByVisitSCI(d, vars = "hb", type = "plot",
                                   word_output = TRUE),
               "'word_output' saves the table")
  expect_error(summarizeByVisitSCI(d, vars = "hb", plot_cont = "violin"),
               "should be one of")
})

# The texts of the footnotes drawn by 'expr' (the grey mtext in the corner)
footnotes <- function(expr) {
  mt <- drawn_calls(expr, "C_mtext")
  unlist(lapply(Filter(function(a) identical(a[[9]], "grey30"), mt),
                `[[`, 1))
}

test_that("stat_footnote shows what each plot displays", {
  d <- hb_fixture()
  expect_null(footnotes(lineplotSCI(d, hb, visit_nr, Arm)))
  expect_equal(footnotes(lineplotSCI(d, hb, visit_nr, Arm, stat_footnote = TRUE)),
               "Median (Q1, Q3)")
  expect_equal(footnotes(lineplotSCI(d, hb, visit_nr, Arm, stat = "mean",
                                     interval = "sd", stat_footnote = TRUE)),
               "Mean \u00b1 SD")
  expect_equal(footnotes(lineplotSCI(d, hb, visit_nr, Arm, interval = "ci",
                                     stat_footnote = TRUE)),
               "Median (95% CI)")
  expect_equal(footnotes(boxplotSCI(d, hb, visit_nr, group = Arm,
                                    stat_footnote = TRUE)),
               "Box: median (Q1, Q3); whiskers: up to 1.5 \u00d7 IQR")
  expect_equal(footnotes(boxplotSCI(d, hb, visit_nr, group = Arm, range = 0,
                                    stat_footnote = TRUE)),
               "Box: median (Q1, Q3); whiskers: Min, Max")
  expect_equal(footnotes(stackedbarplotSCI(d, hb_cat, visit_nr, group = Arm,
                                           stat_footnote = TRUE)),
               "% of patients with a non-missing value")
  expect_equal(footnotes(stackedbarplotSCI(d, hb_cat, visit_nr, group = Arm,
                                           missing = TRUE, stat_footnote = TRUE)),
               "% of patients, including missing values")
  # a text of one's own
  expect_equal(footnotes(stackedbarplotSCI(d, hb_cat, visit_nr, group = Arm,
                                           stat_footnote = "Source: trial X")),
               "Source: trial X")
  expect_error(lineplotSCI(d, hb, visit_nr, stat_footnote = 1),
               "'stat_footnote' must be TRUE, FALSE or a character string")
})

test_that("the footnote takes the same space in every plot type", {
  d <- hb_fixture()
  line_of <- function(expr) {
    mt <- drawn_calls(expr, "C_mtext")
    Filter(function(a) identical(a[[9]], "grey30"), mt)[[1]][[3]]
  }
  box  <- line_of(boxplotSCI(d, hb, visit_nr, group = Arm, risk_table = TRUE,
                             stat_footnote = TRUE))
  line <- line_of(lineplotSCI(d, hb, visit_nr, Arm, risk_table = TRUE,
                              stat_footnote = TRUE))
  expect_equal(box, line)
  # the stacked bar sits lower by its row of group labels, as without footnote
  bar  <- line_of(stackedbarplotSCI(d, hb_cat, visit_nr, group = Arm,
                                    risk_table = TRUE, stat_footnote = TRUE))
  expect_equal(bar, box + 0.8)
})

test_that("the wrapper footnotes follow the table's statistic", {
  d <- sbv_fixture()
  res <- summarizeByVisitSCI(d, vars = c("hb", "hb_cat"), group = "Arm",
                             visit = "visit", order = "visit_nr",
                             stat_cont = "mean_se", as_flex_table = FALSE,
                             plot_args = list(stat_footnote = TRUE))
  expect_equal(footnotes(print(res$plots$hb)), "Mean \u00b1 SE")
  expect_equal(footnotes(print(res$plots$hb_cat)),
               "% of patients, including missing values")
  # plotByVisit() reads the statistic off the table
  tbl <- summaryByVisit(d, vars = "hb", group = "Arm", visit = "visit",
                        stat_cont = "median_range", as_flex_table = FALSE)
  expect_equal(footnotes(plotByVisit(tbl, stat_footnote = TRUE)),
               "Median (Min, Max)")
})
