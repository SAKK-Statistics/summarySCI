# The plotting wrapper reads its numbers off the printed summaryByVisit table.
# These tests check every plotted number against the raw data, so that a
# parsing error (or an error in the table itself) cannot go unnoticed.

# Truth for every group x visit cell of a continuous plot
continuous_truth <- function(d, var, group_col, res, fun) {
  out <- lapply(c("centre", "lower", "upper", "n"), function(k) res$centre * NA)
  names(out) <- c("centre", "lower", "upper", "n")
  for (g in rownames(res$centre)) for (v in colnames(res$centre)) {
    x <- cell(d, var, group_col, g, v)
    s <- fun(x)
    out$centre[g, v] <- s[1]
    out$lower[g, v]  <- s[2]
    out$upper[g, v]  <- s[3]
    out$n[g, v]      <- length(x)
  }
  out
}

check_continuous <- function(d, var, group_col, stat_cont, fun, digits = 2,
                             k = 1, ...) {
  tbl <- summaryByVisit(d, vars = var, group = group_col, visit = "visit",
                        stat_cont = stat_cont, digits_cont = digits, ...)
  res <- draw_quietly(plotByVisit(tbl))
  truth <- continuous_truth(d, var, group_col, res, fun)
  expect_equal(res$type, "continuous")
  expect_within_rounding(res$centre, truth$centre, digits)
  expect_within_rounding(res$lower, truth$lower, digits, k)
  expect_within_rounding(res$upper, truth$upper, digits, k)
  expect_equal(unname(res$n), unname(truth$n))
  invisible(res)
}

test_that("median and range are read correctly", {
  d <- hb_fixture()
  check_continuous(d, "hb", "Arm", "median_range",
                   function(x) c(stats::median(x), range(x)))
})

test_that("median and IQR are read correctly (gtsummary uses quantile type 2)", {
  d <- hb_fixture()
  check_continuous(d, "hb", "Arm", "median_IQR",
                   function(x) c(stats::median(x),
                                 stats::quantile(x, c(0.25, 0.75), type = 2)))
})

test_that("mean +/- SD is read correctly", {
  d <- hb_fixture()
  # both the mean and the SD are rounded, hence twice the tolerance
  check_continuous(d, "hb", "Arm", "mean_sd",
                   function(x) mean(x) + c(0, -1, 1) * stats::sd(x), k = 2)
})

test_that("mean +/- SE is read correctly", {
  d <- hb_fixture()
  check_continuous(d, "hb", "Arm", "mean_se",
                   function(x) mean(x) + c(0, -1, 1) * stats::sd(x) / sqrt(length(x)),
                   k = 2)
})

test_that("geometric mean is read, without an interval", {
  d <- hb_fixture()
  check_continuous(d, "hb", "Arm", "geomMean_sd",
                   function(x) c(exp(mean(log(x))), NA, NA))
})

test_that("ungrouped tables give a single 'Overall' line", {
  d <- hb_fixture()
  res <- check_continuous(d, "hb", NULL, "median_range",
                          function(x) c(stats::median(x), range(x)))
  expect_equal(rownames(res$centre), "Overall")
})

test_that("three arms, thousands separators and negative values are parsed", {
  d <- three_arm_fixture()
  res <- check_continuous(d, "value", "arm", "median_range",
                          function(x) c(stats::median(x), range(x)))
  expect_equal(rownames(res$centre), c("Arm A", "Arm B", "Arm C"))
  expect_true(any(grepl("[0-9],[0-9]", res$summary$stat)))   # "1,234.56"
  expect_true(any(res$lower < 0))
})

test_that("visits are plotted in the order of the table rows", {
  d <- hb_fixture()
  tbl <- summaryByVisit(d, vars = "hb", group = "Arm", visit = "visit")
  res <- draw_quietly(plotByVisit(tbl))
  rows <- tbl$table_body$label[tbl$table_body$row_type %in% "label"]
  expect_equal(colnames(res$centre), unique(rows))
})

# --- Categorical -------------------------------------------------------------

check_categorical <- function(d, var, group_col, ...) {
  tbl <- summaryByVisit(d, vars = var, group = group_col, visit = "visit", ...)
  res <- draw_quietly(plotByVisit(tbl))
  expect_equal(res$type, "categorical")
  for (g in dimnames(res$counts)[[2]]) for (v in dimnames(res$counts)[[3]]) {
    x <- cell(d, var, group_col, g, v)
    truth <- table(factor(x, levels = dimnames(res$counts)[[1]]))
    expect_equal(as.vector(res$counts[, g, v]), as.vector(truth),
                 label = paste("counts", g, v))
    expect_equal(res$n[g, v], length(x), label = paste("n", g, v))
    expect_equal(as.vector(res$percent[, g, v]),
                 as.vector(100 * truth / length(x)))
  }
  invisible(res)
}

test_that("categorical counts and percentages are read correctly (n (%))", {
  d <- hb_fixture()
  res <- check_categorical(d, "hb_cat", "Arm")
  expect_setequal(dimnames(res$counts)[[1]], c("Low", "Normal", "High"))
})

test_that("categorical counts are read correctly for 'n' and 'n/N'", {
  d <- hb_fixture()
  check_categorical(d, "hb_cat", "Arm", stat_cat = "n")
  check_categorical(d, "hb_cat", "Arm", stat_cat = "n_N", missing = FALSE)
})

test_that("categorical counts are read correctly without groups and with three arms", {
  check_categorical(hb_fixture(), "hb_cat", NULL)
  check_categorical(three_arm_fixture(), "grade", "arm")
})

test_that("the N column of the table matches the counts it summarises", {
  d <- hb_fixture()
  tbl <- summaryByVisit(d, vars = "hb_cat", group = "Arm", visit = "visit")
  sm <- tidySummaryByVisit(tbl)
  sm <- sm[sm$level != "Missing", ]
  n_from_counts <- tapply(sm$count, paste(sm$group, sm$visit), sum)
  n_in_table    <- tapply(sm$n,     paste(sm$group, sm$visit), unique)
  expect_equal(n_from_counts, n_in_table)
})

test_that("missing values can be shown as a segment of their own", {
  d <- hb_fixture()
  tbl <- summaryByVisit(d, vars = "hb_cat", group = "Arm", visit = "visit")
  res <- draw_quietly(plotByVisit(tbl, missing = TRUE))
  expect_equal(dimnames(res$counts)[[1]][4], "Missing")
  for (g in c("Arm A", "Arm B")) for (v in unique(d$visit)) {
    x <- d$hb_cat[d$Arm == g & d$visit == v]
    expect_equal(res$counts["Missing", g, v], sum(is.na(x)))
    expect_equal(res$n[g, v], length(x))
  }
  # with missing values included, the percentages are those printed
  printed <- as.numeric(sub(".*\\((.*)%\\)", "\\1", res$summary$stat))
  idx <- cbind(match(res$summary$level, dimnames(res$counts)[[1]]),
               match(res$summary$group, dimnames(res$counts)[[2]]),
               match(res$summary$visit, dimnames(res$counts)[[3]]))
  expect_within_rounding(res$percent[idx], printed, digits = 1)
})

# --- Tables with several variables and visit groups --------------------------

test_that("one variable is picked from a table with several", {
  d <- hb_fixture()
  tbl <- summaryByVisit(d, vars = c("hb", "hb_cat"), group = "Arm",
                        visit = "visit", visitgroup = "vg")
  sm <- tidySummaryByVisit(tbl)
  expect_equal(unique(sm$variable), c("Hemoglobin", "hb_cat"))
  expect_equal(unique(sm$visitgroup), c("Baseline", "Treatment"))

  cont <- draw_quietly(plotByVisit(tbl, var = "Hemoglobin"))
  cat  <- draw_quietly(plotByVisit(tbl, var = 2))
  expect_equal(cont$type, "continuous")
  expect_equal(cat$type, "categorical")

  # the same numbers as when the variables are tabulated on their own
  alone <- summaryByVisit(d, vars = "hb", group = "Arm", visit = "visit")
  expect_equal(cont$centre, draw_quietly(plotByVisit(alone))$centre[, colnames(cont$centre)])
  alone <- summaryByVisit(d, vars = "hb_cat", group = "Arm", visit = "visit")
  expect_equal(cat$counts,
               draw_quietly(plotByVisit(alone))$counts[dimnames(cat$counts)[[1]], , dimnames(cat$counts)[[3]]])
})

test_that("plotByVisit agrees with the raw-data plotting functions", {
  d <- hb_fixture()
  tbl <- summaryByVisit(d, vars = c("hb", "hb_cat"), group = "Arm",
                        visit = "visit", digits_cont = 2)

  from_tbl <- draw_quietly(plotByVisit(tbl, var = "hb_cat"))
  from_raw <- draw_quietly(stackedbarplotSCI(d, hb_cat, time = visit_nr,
                                             time_labels = visit, group = Arm))
  dimnames(from_raw$counts)[[3]] <- unique(d$visit[order(d$visit_nr)])
  expect_equal(from_tbl$counts,
               from_raw$counts[dimnames(from_tbl$counts)[[1]], , dimnames(from_tbl$counts)[[3]]])

  from_tbl <- draw_quietly(plotByVisit(tbl, var = "Hemoglobin"))
  from_raw <- draw_quietly(lineplotSCI(d, hb, visit_nr, Arm, visit = "visit",
                                       stat = "median", interval = "range"))
  colnames(from_raw$centre) <- unique(d$visit[order(d$visit_nr)])
  cols <- colnames(from_tbl$centre)
  expect_within_rounding(from_tbl$centre, from_raw$centre[, cols], digits = 2)
  expect_equal(unname(from_tbl$n), unname(from_raw$n[, match(cols, colnames(from_raw$centre))]))
})

# --- Errors ------------------------------------------------------------------

test_that("informative errors", {
  d <- hb_fixture()
  tbl <- summaryByVisit(d, vars = c("hb", "hb_cat"), visit = "visit")
  expect_error(plotByVisit(tbl), "several variables")
  expect_error(plotByVisit(tbl, var = "nope"), "not found")
  expect_error(plotByVisit(tbl, var = 3), "between 1 and 2")
  expect_error(plotByVisit(tbl, stat_cont = "median"), "must be one of")
  flex <- summaryByVisit(d, vars = "hb", visit = "visit", as_flex_table = TRUE)
  expect_error(plotByVisit(flex), "as_flex_table = FALSE")
  expect_error(plotByVisit(data.frame()), "summaryByVisit")
})

# --- Parsing helpers ---------------------------------------------------------

test_that("printed numbers are parsed", {
  expect_equal(parse_continuous("1,234.5 (-2,345.6, 5,678.9)", "median_range"),
               list(centre = 1234.5, lower = -2345.6, upper = 5678.9))
  expect_equal(parse_continuous("10.0 (2.5)", "mean_sd"),
               list(centre = 10, lower = 7.5, upper = 12.5))
  expect_equal(parse_continuous("NA (NA, NA)", "median_IQR"),
               list(centre = NA_real_, lower = NA_real_, upper = NA_real_))
  expect_equal(parse_continuous("−1.0 (−2.0, 0.5)", "median_range")$lower, -2)
  expect_equal(parse_count(c("1,234.0 (12.5%)", "18.0/60.0", "7", "0 (NA%)")),
               c(1234, 18, 7, 0))
})
