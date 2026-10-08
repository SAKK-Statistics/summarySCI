
testthat::test_that("Error when no data given", {
  testthat::expect_error(summaryTable(data = NULL))
})




testthat::test_that("median and range are correct", {
  trial <- gtsummary::trial
  tbl <- summaryTable(data = trial,
                      vars = "age",
                      as_flex_table = FALSE,
                      digits_cont = 0)

  summary_age <- summary(trial$age)
  median_1 <- round(summary_age["Median"], 0)
  min_1 <- round(summary_age["Min."], 0)
  max_1 <- round(summary_age["Max."], 0)

  median_range_fct <- tbl[["table_body"]][["stat_1"]][[1]]
  median_range_truth <- paste0(median_1, " (",min_1, ", ", max_1, ")")
  testthat::expect_equal(median_range_fct,median_range_truth )
})




testthat::test_that("Number of non-missing observations is correct", {
  trial <- gtsummary::trial

  tbl_5 <- summaryTable(data = trial,
                        vars = "age",
                        as_flex_table = FALSE,
                        add_n = TRUE)

  n_noMissing_fct <- as.numeric(tbl_5[["table_body"]][["add_n_stat_1"]][1])
  n_noMissing_truth <- sum(!is.na(trial$age))
  testthat::expect_equal(n_noMissing_fct, n_noMissing_truth)
})

testthat::test_that("Row missing if missing is true", {
  trial <- gtsummary::trial

tbl6 <- summaryTable(data = trial,
                     vars = "response",
                     group = "grade",
                     as_flex_table = FALSE,
                     add_n = TRUE,
                     overall = TRUE,
                     missing = TRUE)


isMissing <- "Missing" %in% tbl6[["table_body"]][["label"]]


testthat::expect_true(isMissing)
})


testthat::test_that("N without missing add up to overall N without missing", {
  trial <- gtsummary::trial
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

testthat::expect_equal(sum(n1, n2, n3), n_overall)


})



# ============================================================================ #
# Helpers for the tests below
# (can be moved to tests/testthat/helper-summaryTable.R, loaded automatically)
# ============================================================================ #

# Unrounded p-value of a variable, taken from the gtsummary table_body
get_p <- function(tbl, var) {
  tb   <- tbl[["table_body"]]
  pcol <- grep("^p.value", names(tb), value = TRUE)[1]
  if (is.na(pcol)) return(NULL)
  unname(as.numeric(unlist(tb[[pcol]][tb$variable == var & tb$row_type == "label"])))
}

# Displayed cell of a variable: row_type "label" (continuous / dichotomous)
# or the row of a given level (categorical)
get_cell <- function(tbl, var, col = "stat_1", level = NULL) {
  tb <- tbl[["table_body"]]
  if (is.null(level)) {
    tb[[col]][tb$variable == var & tb$row_type == "label"]
  } else {
    tb[[col]][tb$variable == var & tb$row_type == "level" & tb$label == level]
  }
}

# All numbers contained in a cell, e.g. "47 (38, 57)" -> c(47, 38, 57)
nums <- function(x) {
  as.numeric(regmatches(x, gregexpr("-?[0-9]+\\.?[0-9]*", x))[[1]])
}

# Numbers in a cell equal to reference values up to rounding to `digits`
expect_rounded <- function(cell, ref, digits) {
  got <- nums(cell)
  expect_equal(length(got), length(ref), label = paste0("number of values in '", cell, "'"))
  expect_true(all(abs(got - ref) <= 0.5 * 10^(-digits) + 1e-8),
              label = paste0("'", cell, "' vs reference ", paste(signif(ref, 6), collapse = ", ")))
}

# Complete cases for one variable and the group (what the tests should use)
cc <- function(data, var, group) data[!is.na(data[[var]]) & !is.na(data[[group]]), ]

# The rule documented for test_cat = NULL
auto_cat_p <- function(x, g) {
  tab <- table(x, g)
  expected <- suppressWarnings(stats::chisq.test(tab)$expected)
  if (all(expected >= 5)) suppressWarnings(stats::chisq.test(tab, correct = FALSE)$p.value)
  else stats::fisher.test(tab)$p.value
}

TOL <- 1e-8


# ============================================================================ #
# P-values: categorical variables
# ============================================================================ #

test_that("Fisher p-values are correct (2 groups)", {
  trial <- gtsummary::trial
  vars  <- c("grade", "stage", "response", "death")

  tbl <- summaryTable(trial, vars = vars, group = "trt", test = TRUE,
                      test_cat = "fisher.test", as_flex_table = FALSE)

  for (v in vars) {
    d   <- cc(trial, v, "trt")
    ref <- stats::fisher.test(table(d[[v]], d$trt))$p.value
    expect_equal(get_p(tbl, v), ref, tolerance = TOL, label = paste("fisher", v))
  }
})

test_that("Fisher p-values are correct (3 groups)", {
  trial <- gtsummary::trial
  vars  <- c("stage", "response", "trt")

  tbl <- summaryTable(trial, vars = vars, group = "grade", test = TRUE,
                      test_cat = "fisher.test", as_flex_table = FALSE)

  for (v in vars) {
    d   <- cc(trial, v, "grade")
    ref <- stats::fisher.test(table(d[[v]], d$grade))$p.value
    expect_equal(get_p(tbl, v), ref, tolerance = TOL, label = paste("fisher 3 groups", v))
  }
})

test_that("Chi-squared p-values are correct (with and without correction)", {
  trial <- gtsummary::trial
  vars  <- c("grade", "stage", "response", "death")

  tbl_c  <- summaryTable(trial, vars = vars, group = "trt", test = TRUE,
                         test_cat = "chisq.test", as_flex_table = FALSE)
  tbl_nc <- summaryTable(trial, vars = vars, group = "trt", test = TRUE,
                         test_cat = "chisq.test.no.correct", as_flex_table = FALSE)

  for (v in vars) {
    d   <- cc(trial, v, "trt")
    tab <- table(d[[v]], d$trt)
    ref_c  <- suppressWarnings(stats::chisq.test(tab, correct = TRUE)$p.value)
    ref_nc <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$p.value)
    expect_equal(get_p(tbl_c,  v), ref_c,  tolerance = TOL, label = paste("chisq", v))
    expect_equal(get_p(tbl_nc, v), ref_nc, tolerance = TOL, label = paste("chisq no correct", v))
  }
})

test_that("test_cat = NULL chooses chisq (no correction) or Fisher by expected counts", {
  set.seed(1)
  trial <- gtsummary::trial
  trial$rare <- factor(sample(c(rep("a", 6), rep("b", nrow(trial) - 6))))  # expected < 5 -> Fisher

  vars <- c("grade", "stage", "rare")
  tbl  <- summaryTable(trial, vars = vars, group = "trt", test = TRUE,
                       test_cat = NULL, as_flex_table = FALSE)

  for (v in vars) {
    d <- cc(trial, v, "trt")
    expect_equal(get_p(tbl, v), auto_cat_p(d[[v]], d$trt), tolerance = TOL,
                 label = paste("test_cat NULL", v))
  }
  # make sure the 'rare' case really is the Fisher branch
  d <- cc(trial, "rare", "trt")
  expect_equal(get_p(tbl, "rare"), stats::fisher.test(table(d$rare, d$trt))$p.value, tolerance = TOL)
})

test_that("Categorical p-value does not depend on missing display options", {
  # Missing values must NOT be part of the test (no "Missing" category in the test)
  trial <- gtsummary::trial
  d     <- cc(trial, "response", "trt")
  ref   <- stats::fisher.test(table(d$response, d$trt))$p.value

  for (m in list(TRUE, FALSE)) for (mp in list(TRUE, FALSE, "both")) {
    tbl <- summaryTable(trial, vars = "response", group = "trt", test = TRUE,
                        missing = m, missing_percent = mp, as_flex_table = FALSE)
    expect_equal(get_p(tbl, "response"), ref, tolerance = TOL,
                 label = paste0("missing=", m, " missing_percent=", mp))
  }
})

test_that("Categorical p-value does not depend on dichotomous_as / ref_level", {
  trial <- gtsummary::trial
  d     <- cc(trial, "response", "trt")
  ref   <- stats::fisher.test(table(d$response, d$trt))$p.value

  tbl_d <- summaryTable(trial, vars = "response", group = "trt", test = TRUE, missing = FALSE,
                        dichotomous_as = "dichotomous", as_flex_table = FALSE)
  tbl_c <- summaryTable(trial, vars = "response", group = "trt", test = TRUE, missing = FALSE,
                        dichotomous_as = "categorical", as_flex_table = FALSE)
  tbl_r <- summaryTable(trial, vars = "response", group = "trt", test = TRUE, missing = FALSE,
                        ref_level = list(response ~ "0"), as_flex_table = FALSE)

  expect_equal(get_p(tbl_d, "response"), ref, tolerance = TOL)
  expect_equal(get_p(tbl_c, "response"), ref, tolerance = TOL)
  expect_equal(get_p(tbl_r, "response"), ref, tolerance = TOL)
})

test_that("Character and logical variables give the same p-value as the factor version", {
  trial <- gtsummary::trial
  trial$grade_chr <- as.character(trial$grade)
  trial$resp_lgl  <- as.logical(trial$response)

  tbl <- summaryTable(trial, vars = c("grade", "grade_chr", "response", "resp_lgl"),
                      group = "trt", test = TRUE, as_flex_table = FALSE)

  expect_equal(get_p(tbl, "grade_chr"), get_p(tbl, "grade"), tolerance = TOL)
  expect_equal(get_p(tbl, "resp_lgl"),  get_p(tbl, "response"), tolerance = TOL)
})


# ============================================================================ #
# P-values: continuous variables
# ============================================================================ #

test_that("Continuous p-values are correct (2 groups)", {
  trial <- gtsummary::trial
  vars  <- c("age", "marker", "ttdeath")

  # NOTE: t.test and oneway.test are compared with the R defaults (Welch, var.equal = FALSE).
  ref_fun <- list(
    t.test       = function(x, g) stats::t.test(x ~ g)$p.value,
    wilcox.test  = function(x, g) suppressWarnings(stats::wilcox.test(x ~ g)$p.value),
    kruskal.test = function(x, g) stats::kruskal.test(x ~ g)$p.value,
    oneway.test  = function(x, g) stats::oneway.test(x ~ g)$p.value
  )

  for (tc in names(ref_fun)) {
    tbl <- summaryTable(trial, vars = vars, group = "trt", test = TRUE,
                        test_cont = tc, as_flex_table = FALSE)
    for (v in vars) {
      d <- cc(trial, v, "trt")
      expect_equal(get_p(tbl, v), ref_fun[[tc]](d[[v]], d$trt), tolerance = TOL,
                   label = paste(tc, v))
    }
  }
})

test_that("Continuous p-values are correct (3 groups)", {
  trial <- gtsummary::trial
  vars  <- c("age", "marker")

  for (tc in c("kruskal.test", "oneway.test")) {
    tbl <- summaryTable(trial, vars = vars, group = "grade", test = TRUE,
                        test_cont = tc, as_flex_table = FALSE)
    for (v in vars) {
      d   <- cc(trial, v, "grade")
      ref <- if (tc == "kruskal.test") stats::kruskal.test(d[[v]] ~ d$grade)$p.value
             else stats::oneway.test(d[[v]] ~ d$grade)$p.value
      expect_equal(get_p(tbl, v), ref, tolerance = TOL, label = paste(tc, "3 groups", v))
    }
  }
})

test_that("Default test_cont: wilcox for 2 groups, kruskal for > 2 groups", {
  trial <- gtsummary::trial

  tbl2 <- summaryTable(trial, vars = "age", group = "trt",   test = TRUE, as_flex_table = FALSE)
  tbl3 <- summaryTable(trial, vars = "age", group = "grade", test = TRUE, as_flex_table = FALSE)

  d2 <- cc(trial, "age", "trt")
  d3 <- cc(trial, "age", "grade")
  expect_equal(get_p(tbl2, "age"), suppressWarnings(stats::wilcox.test(age ~ trt, data = d2)$p.value),
               tolerance = TOL)
  expect_equal(get_p(tbl3, "age"), stats::kruskal.test(age ~ grade, data = d3)$p.value,
               tolerance = TOL)
})

test_that("Continuous p-value does not depend on stat_cont / digits / overall", {
  trial <- gtsummary::trial
  d     <- cc(trial, "age", "trt")
  ref   <- stats::t.test(age ~ trt, data = d)$p.value

  for (sc in c("median_IQR", "median_range", "mean_sd", "mean_se")) {
    tbl <- summaryTable(trial, vars = "age", group = "trt", test = TRUE, test_cont = "t.test",
                        stat_cont = sc, digits_cont = 3, overall = TRUE, as_flex_table = FALSE)
    expect_equal(get_p(tbl, "age"), ref, tolerance = TOL, label = paste("stat_cont", sc))
  }
})

test_that("continuous_as_categorical uses a categorical test", {
  trial <- gtsummary::trial
  trial$n_nodes <- sample(0:3, nrow(trial), replace = TRUE)

  tbl <- summaryTable(trial, vars = "n_nodes", group = "trt", test = TRUE,
                      continuous_as_categorical = "n_nodes", test_cat = "fisher.test",
                      as_flex_table = FALSE)
  ref <- stats::fisher.test(table(trial$n_nodes, trial$trt))$p.value
  expect_equal(get_p(tbl, "n_nodes"), ref, tolerance = TOL)
})



# ============================================================================ #
# P-value display
# ============================================================================ #

test_that("No p-value column when test = FALSE", {
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = c("age", "grade"), group = "trt", as_flex_table = FALSE)
  expect_null(get_p(tbl, "age"))
})

test_that("Displayed p-value matches the unrounded p-value", {
  # summarySCI shows p-values with 2 digits (gtsummary::style_pvalue(digits = 2)).
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = c("age", "marker", "grade", "response"), group = "trt",
                      test = TRUE, as_flex_table = FALSE)
  shown <- gtsummary::as_tibble(tbl, col_labels = FALSE)
  pcol  <- grep("p.value", names(shown), value = TRUE)[1]
  expect_false(is.na(pcol),
               label = paste("p-value column in displayed table; columns are:",
                             paste(names(shown), collapse = ", ")))
  if (is.na(pcol)) return(invisible())

  for (v in c("age", "marker", "grade", "response")) {
    p_shown <- shown[[pcol]][tbl$table_body$variable == v & tbl$table_body$row_type == "label"]
    expect_equal(p_shown, gtsummary::style_pvalue(get_p(tbl, v), digits = 2), label = paste("p shown", v))
  }
})

test_that("Test with a group of 1 level gives an error or no p-value", {
  trial <- gtsummary::trial
  trial$one_arm <- factor("A")
  res <- tryCatch(summaryTable(trial, vars = "age", group = "one_arm", test = TRUE,
                               as_flex_table = FALSE), error = function(e) e)
  expect_true(inherits(res, "error") || all(is.na(get_p(res, "age"))))
})


# ============================================================================ #
# Summary statistics
# ============================================================================ #

test_that("mean (SD), mean (SE), median (IQR) are correct", {
  trial <- gtsummary::trial
  x <- trial$age[!is.na(trial$age)]

  tbl_m  <- summaryTable(trial, vars = "age", stat_cont = "mean_sd",    digits_cont = 2, as_flex_table = FALSE)
  tbl_se <- summaryTable(trial, vars = "age", stat_cont = "mean_se",    digits_cont = 2, as_flex_table = FALSE)
  tbl_q  <- summaryTable(trial, vars = "age", stat_cont = "median_IQR", digits_cont = 2, as_flex_table = FALSE)

  expect_rounded(get_cell(tbl_m,  "age"), c(mean(x), sd(x)), 2)
  expect_rounded(get_cell(tbl_se, "age"), c(mean(x), sd(x) / sqrt(length(x))), 2)
  # gtsummary uses quantile(type = 2) for p25 / p75
  expect_rounded(get_cell(tbl_q,  "age"),
                 c(median(x), stats::quantile(x, c(.25, .75), type = 2)), 2)
})

test_that("Geometric mean is correct", {
  # Only the geometric mean is checked; adapt for the dispersion measure used.
  trial <- gtsummary::trial
  x   <- trial$age[!is.na(trial$age)]
  tbl <- summaryTable(trial, vars = "age", stat_cont = "geomMean_sd", digits_cont = 2,
                      as_flex_table = FALSE)
  expect_true(abs(nums(get_cell(tbl, "age"))[1] - exp(mean(log(x)))) <= 0.005 + 1e-8)
})

test_that("Statistics by group are correct", {
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = "marker", group = "grade", stat_cont = "median_range",
                      digits_cont = 2, as_flex_table = FALSE)
  lv <- levels(trial$grade)
  for (i in seq_along(lv)) {
    x <- trial$marker[trial$grade == lv[i] & !is.na(trial$marker)]
    expect_rounded(get_cell(tbl, "marker", col = paste0("stat_", i)),
                   c(median(x), min(x), max(x)), 2)
  }
})

test_that("n (%) is correct for each level (variable without missing)", {
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = "grade", group = "trt", stat_cat = "n_percent",
                      digits_cat = 1, as_flex_table = FALSE)
  arms <- levels(factor(trial$trt))
  for (i in seq_along(arms)) for (lv in levels(trial$grade)) {
    n_ref   <- sum(trial$grade == lv & trial$trt == arms[i])
    pct_ref <- 100 * n_ref / sum(trial$trt == arms[i])
    expect_rounded(get_cell(tbl, "grade", col = paste0("stat_", i), level = lv),
                   c(n_ref, pct_ref), 1)
  }
})

test_that("stat_cat = 'n' and 'n_N' are correct", {
  trial <- gtsummary::trial
  tbl_n  <- summaryTable(trial, vars = "grade", stat_cat = "n",   as_flex_table = FALSE)
  tbl_nN <- summaryTable(trial, vars = "grade", stat_cat = "n_N", as_flex_table = FALSE)
  for (lv in levels(trial$grade)) {
    n_ref <- sum(trial$grade == lv)
    expect_equal(nums(get_cell(tbl_n, "grade", level = lv)), n_ref)
    expect_equal(nums(get_cell(tbl_nN, "grade", level = lv))[1:2], c(n_ref, nrow(trial)))
  }
})

test_that("Overall column equals the ungrouped table", {
  trial <- gtsummary::trial
  tbl_g <- summaryTable(trial, vars = c("age", "grade"), group = "trt", overall = TRUE,
                        as_flex_table = FALSE)
  tbl_u <- summaryTable(trial, vars = c("age", "grade"), as_flex_table = FALSE)
  expect_equal(tbl_g$table_body$stat_0, tbl_u$table_body$stat_1)
})

test_that("Missing row shows the correct number of missing values", {
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = "age", missing = TRUE, as_flex_table = FALSE)
  tb  <- tbl$table_body
  miss_cell <- tb$stat_1[tb$variable == "age" & tb$row_type == "missing"]
  expect_equal(nums(miss_cell)[1], sum(is.na(trial$age)))
})

test_that("missing_text is used", {
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = "age", missing_text = "Not reported", as_flex_table = FALSE)
  expect_true("Not reported" %in% tbl$table_body$label)
})

test_that("No missing row when missing = FALSE", {
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = c("age", "response"), missing = FALSE, as_flex_table = FALSE)
  expect_false(any(tbl$table_body$row_type == "missing"))
})

test_that("digits_cont is respected", {
  trial <- gtsummary::trial
  for (dg in 0:3) {
    tbl  <- summaryTable(trial, vars = "marker", stat_cont = "mean_sd", digits_cont = dg,
                         as_flex_table = FALSE)
    cell <- get_cell(tbl, "marker")
    decimals <- nchar(sub("^[^.]*\\.?", "", regmatches(cell, gregexpr("[0-9.]+", cell))[[1]]))
    expect_true(all(decimals == dg), label = paste("digits_cont =", dg, ":", cell))
  }
})


# ============================================================================ #
# Confidence intervals
# ============================================================================ #

get_ci <- function(tbl, var, level = NULL) {
  tb    <- tbl[["table_body"]]
  cicol <- grep("^ci", names(tb), value = TRUE)[1]
  get_cell(tbl, var, col = cicol, level = level)
}

test_that("Clopper-Pearson CI is correct", {
  trial <- gtsummary::trial
  x <- trial$response[!is.na(trial$response)]
  tbl <- summaryTable(trial, vars = "response", ci = TRUE, ci_cat = "clopper.pearson",
                      missing = FALSE, digits_cat = 1, as_flex_table = FALSE)
  ref <- 100 * stats::binom.test(sum(x == 1), length(x))$conf.int
  expect_rounded(get_ci(tbl, "response"), ref, 1)
})

test_that("Wilson CI is correct", {
  trial <- gtsummary::trial
  x <- trial$response[!is.na(trial$response)]
  tbl_w  <- summaryTable(trial, vars = "response", ci = TRUE, ci_cat = "wilson",
                         missing = FALSE, digits_cat = 1, as_flex_table = FALSE)
  tbl_nc <- summaryTable(trial, vars = "response", ci = TRUE, ci_cat = "wilson.no.correct",
                         missing = FALSE, digits_cat = 1, as_flex_table = FALSE)
  expect_rounded(get_ci(tbl_w,  "response"), 100 * stats::prop.test(sum(x == 1), length(x), correct = TRUE)$conf.int,  1)
  expect_rounded(get_ci(tbl_nc, "response"), 100 * stats::prop.test(sum(x == 1), length(x), correct = FALSE)$conf.int, 1)
})

test_that("CI for continuous variables is correct (t.test and wilcox.test)", {
  trial <- gtsummary::trial
  x <- trial$age[!is.na(trial$age)]
  tbl_t <- summaryTable(trial, vars = "age", ci = TRUE, ci_cont = "t.test",
                        digits_cont = 2, as_flex_table = FALSE)
  tbl_w <- summaryTable(trial, vars = "age", ci = TRUE, ci_cont = "wilcox.test",
                        digits_cont = 2, as_flex_table = FALSE)
  expect_rounded(get_ci(tbl_t, "age"), stats::t.test(x)$conf.int, 2)
  expect_rounded(get_ci(tbl_w, "age"),
                 suppressWarnings(stats::wilcox.test(x, conf.int = TRUE)$conf.int), 2)
})

test_that("conf_level is used", {
  trial <- gtsummary::trial
  x <- trial$age[!is.na(trial$age)]
  tbl <- summaryTable(trial, vars = "age", ci = TRUE, ci_cont = "t.test", conf_level = 0.9,
                      digits_cont = 2, as_flex_table = FALSE)
  expect_rounded(get_ci(tbl, "age"), stats::t.test(x, conf.level = 0.9)$conf.int, 2)
})


# ============================================================================ #
# Variable types / levels
# ============================================================================ #

test_that("Dichotomous variable is shown on one row with the chosen ref_level", {
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = "response", missing = FALSE, dichotomous_as = "dichotomous",
                      ref_level = list(response ~ "0"), as_flex_table = FALSE)
  tb <- tbl$table_body[tbl$table_body$variable == "response", ]
  expect_equal(nrow(tb), 1)
  x <- trial$response[!is.na(trial$response)]
  expect_equal(nums(tb$stat_1)[1], sum(x == 0))
})

test_that("dichotomous_as = 'categorical' shows one row per level", {
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = "response", missing = FALSE, dichotomous_as = "categorical",
                      as_flex_table = FALSE)
  expect_equal(sum(tbl$table_body$variable == "response" & tbl$table_body$row_type == "level"), 2)
})


test_that("Variable labels from attributes and from `labels` are used", {
  trial <- gtsummary::trial
  tbl <- summaryTable(trial, vars = c("age", "grade"), labels = list(grade = "Tumour grade"),
                      as_flex_table = FALSE)
  expect_true("Tumour grade" %in% tbl$table_body$label)
  expect_true(attr(trial$age, "label") %in% tbl$table_body$label)
})


# ============================================================================ #
# Input checks
# ============================================================================ #


test_that("Date variables give a clear error", {
  trial <- gtsummary::trial
  trial$date <- as.Date("2024-01-01") + seq_len(nrow(trial))
  expect_error(summaryTable(trial, vars = "date"), "not adapted for date variables")
})

test_that("Unknown vars / group give an error", {
  trial <- gtsummary::trial
  expect_error(summaryTable(trial, vars = "foo"))
  expect_error(summaryTable(trial, vars = "age", group = "foo"))
})

test_that("Return class depends on as_flex_table", {
  trial <- gtsummary::trial
  expect_s3_class(summaryTable(trial, vars = "age"), "flextable")
  expect_s3_class(summaryTable(trial, vars = "age", as_flex_table = FALSE), "gtsummary")
})

test_that("Word file is written", {
  trial <- gtsummary::trial
  f <- tempfile(fileext = ".docx")
  summaryTable(trial, vars = "age", word_output = TRUE, file_name = f)
  expect_true(file.exists(f))
})
