# Data: one binary column per possible entry ---------------------------------

lv <- data.frame(
  nausea  = c(1, 0, 1, 1, 0, 0, 1, 0),
  fatigue = c("yes", "no", "yes", "no", NA, "no", "yes", "yes"),
  ecog    = c(0, 1, 2, 3, 1, 0, 2, 1),
  arm     = rep(c("A", "B"), 4)
)

lv_tbl <- function(...) summaryLevels(..., as_flex_table = FALSE)


# Input check ------------------------------------------------------------------

test_that("non-binary variables give an informative error", {
  expect_error(lv_tbl(lv, vars = c("nausea", "ecog"), group = "arm"),
               "Not binary: `ecog` \\(values: 0, 1, 2, 3\\)")
})

test_that("binary variables (0/1, yes/no with NA) are accepted", {
  expect_no_error(lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm"))
})

test_that("a non-binary variable is accepted when levels is given", {
  expect_no_error(lv_tbl(lv, vars = "ecog", group = "arm", levels = c(2, 3)))
})


# levels -----------------------------------------------------------------------

test_that("several values in levels are all counted as present", {
  d <- data.frame(x = c("yes", "1", "Yes", "no", "0", "no"))
  tbl <- lv_tbl(d, vars = "x", levels = c("1", "yes", "Yes"))
  expect_equal(tbl$table_body$stat_0[tbl$table_body$variable == "x"], "3 (50%)")
})


# Column order -----------------------------------------------------------------

test_that("Overall column is the last column", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm", overall = TRUE)
  cols <- names(tbl$table_body)
  expect_gt(match("stat_0", cols), match("stat_2", cols))
})

test_that("p-value comes before the Overall column", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm",
                overall = TRUE, test = TRUE)
  cols <- names(tbl$table_body)
  expect_gt(match("p.value", cols), match("stat_2", cols))
  expect_lt(match("p.value", cols), match("stat_0", cols))
})


test_that("with CIs, the p-value still comes before N and the Overall column", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm",
                overall = TRUE, test = TRUE, ci = TRUE)
  cols <- names(tbl$table_body)
  expect_equal(match("ci_stat_1", cols), match("stat_1", cols) + 1)  # CI next to its group
  expect_equal(match("ci_stat_2", cols), match("stat_2", cols) + 1)
  expect_gt(match("p.value", cols), match("ci_stat_2", cols))        # p after the groups
  expect_equal(match("n", cols), match("p.value", cols) + 1)         # then N
  expect_equal(match("stat_0", cols), match("n", cols) + 1)          # then Overall
  expect_equal(match("ci_stat_0", cols), match("stat_0", cols) + 1)  # then its CI
})

test_that("with CIs but without test, Overall and its CI are last", {
  tbl <- lv_tbl(lv, vars = "nausea", group = "arm", overall = TRUE, ci = TRUE)
  cols <- names(tbl$table_body)
  expect_equal(utils::tail(cols, 2), c("stat_0", "ci_stat_0"))
})


test_that("N columns are correctly placed in all combinations of options", {
  for (grp in list(NULL, "arm")) for (ov in c(FALSE, TRUE)) for (te in c(FALSE, TRUE))
    for (ci in c(FALSE, TRUE)) for (an in c(FALSE, TRUE)) {
      if (is.null(grp) && (te || ov)) next
      lab <- paste("group", !is.null(grp), "overall", ov, "test", te, "ci", ci, "add_n", an)
      tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = grp, overall = ov,
                    test = te, ci = ci, add_n = an)
      cols <- names(tbl$table_body)
      pos  <- function(x) match(x, cols)
      grp_cols <- grep("^stat_[1-9]", cols, value = TRUE)
      for (sc in grp_cols) {
        k <- sub("stat_", "", sc)
        if (an) expect_equal(pos(paste0("add_n_stat_", k)), pos(sc) - 1, label = lab)
        if (ci) expect_equal(pos(paste0("ci_stat_", k)), pos(sc) + 1, label = lab)
      }
      if ("stat_0" %in% cols) {
        last_grp <- max(c(0, pos(grp_cols), pos(grep("^ci_stat_[1-9]", cols, value = TRUE))))
        expect_gt(pos("stat_0"), last_grp, label = lab)                  # Overall after groups
        if (an) expect_equal(pos("n"), pos("stat_0") - 1, label = lab)   # N right before it
        if (ci) expect_equal(pos("ci_stat_0"), pos("stat_0") + 1, label = lab)
        if (te) expect_lt(pos("p.value"), pos(if (an) "n" else "stat_0"), label = lab)
      }
      if (!an) expect_false(any(c("n", "add_n_stat_1") %in% cols), label = lab)
    }
})

test_that("each N belongs to its own group, also for mixed upper/lower case groups", {
  # "B" sorts before "a" in the C locale (dplyr) but after it in most others
  d <- data.frame(x   = c(1, 0, 1, 1, 0, 1),
                  arm = c("B", "a", "a", "a", "B", "a"))
  tbl <- lv_tbl(d, vars = "x", group = "arm")
  hdr <- tbl$table_styling$header
  for (k in 1:2) {
    h <- hdr$label[hdr$column == paste0("stat_", k)]
    n_header <- as.numeric(sub(".*N = ([0-9]+).*", "\\1", h))
    expect_equal(as.numeric(tbl$table_body[[paste0("add_n_stat_", k)]][1]), n_header,
                 label = h)
  }
})


# add_n ------------------------------------------------------------------------

test_that("add_n adds an N column before each group column", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm")
  cols <- names(tbl$table_body)
  expect_equal(match("add_n_stat_1", cols) + 1, match("stat_1", cols))
  expect_equal(match("add_n_stat_2", cols) + 1, match("stat_2", cols))
  expect_equal(as.numeric(unique(tbl$table_body$add_n_stat_1)), 4)
  expect_equal(as.numeric(unique(tbl$table_body$add_n_stat_2)), 4)
})

test_that("with overall, total N is just before Overall and after the p-value", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm",
                overall = TRUE, test = TRUE)
  cols <- names(tbl$table_body)
  expect_equal(match("n", cols) + 1, match("stat_0", cols))
  expect_lt(match("p.value", cols), match("n", cols))
  expect_equal(as.numeric(unique(tbl$table_body$n)), 8)
})

test_that("without group, an N column is added", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"))
  expect_true("n" %in% names(tbl$table_body))
})

test_that("N columns have a footnote explaining N", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm", overall = TRUE)
  expect_true(any(grepl("missing values counted as absent",
                        unlist(tbl$table_styling), fixed = TRUE)))
})

test_that("add_n = FALSE adds no N columns", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm",
                overall = TRUE, add_n = FALSE)
  cols <- names(tbl$table_body)
  expect_false(any(c("n", "add_n_stat_1", "add_n_stat_2") %in% cols))
})


# Confidence intervals ---------------------------------------------------------

ci_text <- function(tbl) {
  x <- unlist(tbl$table_body[grepl("^ci_stat", names(tbl$table_body))])
  x[!is.na(x) & nzchar(x)]
}

test_that("Clopper-Pearson is the default CI and matches binom.test", {
  d <- data.frame(x = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0))
  ci_default <- ci_text(lv_tbl(d, vars = "x", ci = TRUE))
  expect_equal(ci_default,
               ci_text(lv_tbl(d, vars = "x", ci = TRUE, ci_cat = "clopper.pearson")))
  expect_false(identical(ci_default,
                         ci_text(lv_tbl(d, vars = "x", ci = TRUE, ci_cat = "wilson"))))

  # 3/10: exact CI 6.7% to 65.2%
  nums <- as.numeric(regmatches(ci_default[1], gregexpr("[0-9.]+", ci_default[1]))[[1]])
  expect_true(all(abs(nums - 100 * stats::binom.test(3, 10)$conf.int) < 1))
})

test_that("both spellings of Clopper-Pearson and other methods work", {
  for (m in c("clopper.pearson", "clopper-pearson", "wilson", "jeffreys")) {
    expect_no_error(lv_tbl(lv, vars = "nausea", group = "arm", ci = TRUE, ci_cat = m))
  }
})

test_that("an unknown CI method gives an error", {
  expect_error(lv_tbl(lv, vars = "nausea", ci = TRUE, ci_cat = "foo"),
               "does not exist")
})


# Counts and percentages -------------------------------------------------------
# lv: arm A = rows 1, 3, 5, 7; arm B = rows 2, 4, 6, 8
#   nausea:  A 3/4, B 1/4, overall 4/8
#   fatigue: A 3/4 (one NA counted as absent), B 1/4, overall 4/8

lv_cell <- function(tbl, var, col) {
  tb <- tbl$table_body
  tb[[col]][tb$variable == var][1]
}

test_that("n (%) per group and overall is correct", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm", overall = TRUE)
  expect_equal(lv_cell(tbl, "nausea", "stat_1"), "3 (75%)")
  expect_equal(lv_cell(tbl, "nausea", "stat_2"), "1 (25%)")
  expect_equal(lv_cell(tbl, "nausea", "stat_0"), "4 (50%)")
  expect_equal(lv_cell(tbl, "fatigue", "stat_1"), "3 (75%)")
  expect_equal(lv_cell(tbl, "fatigue", "stat_2"), "1 (25%)")
})

test_that("missing values are counted as absent (denominator = all patients)", {
  d <- data.frame(x = c(1, NA, NA, 0))
  expect_equal(lv_cell(lv_tbl(d, vars = "x"), "x", "stat_0"), "1 (25%)")
})

test_that("yes/no, Yes/No, TRUE/FALSE and 1/0 give the same counts", {
  x <- c(1, 0, 1, 1, 0)
  d <- data.frame(num = x,
                  yn  = ifelse(x == 1, "yes", "no"),
                  YN  = ifelse(x == 1, "Yes", "No"),
                  mix = c("Yes", "no", "yes", "YES", "No"),
                  lgl = x == 1)
  tbl <- lv_tbl(d, vars = names(d))
  for (v in names(d)) expect_equal(lv_cell(tbl, v, "stat_0"), "3 (60%)", label = v)
})

test_that("levels with a numeric code (1 = yes, 2 = no) works", {
  d <- data.frame(x = c(1, 2, 2, 1, 2))
  expect_error(lv_tbl(d, vars = "x"), "Not binary")
  expect_equal(lv_cell(lv_tbl(d, vars = "x", levels = 1), "x", "stat_0"), "2 (40%)")
})

test_that("stat_cat = 'n' and 'n_N' are correct", {
  expect_equal(lv_cell(lv_tbl(lv, vars = "nausea", group = "arm", stat_cat = "n"),
                       "nausea", "stat_1"), "3")
  expect_equal(lv_cell(lv_tbl(lv, vars = "nausea", group = "arm", stat_cat = "n_N"),
                       "nausea", "stat_1"), "3/4")
})

test_that("digits_cat is used for the percentages, not for the counts", {
  tbl <- lv_tbl(lv, vars = "nausea", group = "arm", digits_cat = 1)
  expect_equal(lv_cell(tbl, "nausea", "stat_1"), "3 (75.0%)")
  tbl <- lv_tbl(lv, vars = "nausea", group = "arm", stat_cat = "n_N", digits_cat = 1)
  expect_equal(lv_cell(tbl, "nausea", "stat_1"), "3/4")
})

test_that("works with three groups", {
  d <- data.frame(x   = c(1, 0, 1, 1, 0, 1, 0, 0, 1),
                  arm = rep(c("A", "B", "C"), 3))
  # A: 1, 1, 0   B: 0, 0, 0   C: 1, 1, 1
  tbl <- lv_tbl(d, vars = "x", group = "arm", overall = TRUE)
  expect_equal(lv_cell(tbl, "x", "stat_1"), "2 (67%)")
  expect_equal(lv_cell(tbl, "x", "stat_2"), "0 (0%)")
  expect_equal(lv_cell(tbl, "x", "stat_3"), "3 (100%)")
  expect_equal(lv_cell(tbl, "x", "stat_0"), "5 (56%)")
})

test_that("a variable that is never present shows 0", {
  d <- data.frame(x = c(0, 0, 0, 0), arm = c("A", "B", "A", "B"))
  tbl <- lv_tbl(d, vars = "x", group = "arm")
  expect_match(lv_cell(tbl, "x", "stat_1"), "^0")
})


# Groups -------------------------------------------------------------------------

test_that("any number of groups works (here 6)", {
  d <- data.frame(x = rep(c(1, 0), 6), arm = rep(paste("Arm", 1:6), each = 2))
  tbl <- lv_tbl(d, vars = "x", group = "arm", overall = TRUE)
  for (k in 1:6) expect_equal(lv_cell(tbl, "x", paste0("stat_", k)), "1 (50%)",
                              label = paste("group", k))
  expect_equal(lv_cell(tbl, "x", "stat_0"), "6 (50%)")
})

test_that("a variable never present shows 0 in every group, also with 5 groups", {
  d <- data.frame(x = rep(0, 10), arm = rep(LETTERS[1:5], 2))
  tbl <- lv_tbl(d, vars = "x", group = "arm", overall = TRUE)
  for (k in 0:5) expect_match(lv_cell(tbl, "x", paste0("stat_", k)), "^0",
                              label = paste("stat", k))
})

test_that("rows with a missing group are removed with a message", {
  d <- data.frame(x = c(1, 1, 0, 0, 1), arm = c("A", "A", "B", "B", NA))
  expect_message(tbl <- lv_tbl(d, vars = "x", group = "arm", overall = TRUE),
                 "1 row\\(s\\) with missing 'arm' were removed")
  expect_equal(lv_cell(tbl, "x", "stat_0"), "2 (50%)")   # 2/4, not 3/5
  expect_false("stat_3" %in% names(tbl$table_body))     # no extra group "0"
})

test_that("group with only one group gives an error", {
  d <- data.frame(x = c(1, 0, 1), arm = "A")
  expect_error(lv_tbl(d, vars = "x", group = "arm"), "at least two groups")
  d$arm[3] <- NA
  d$arm[2] <- "B"
  d$arm[1] <- NA
  expect_error(suppressMessages(lv_tbl(d, vars = "x", group = "arm")), "at least two groups")
})

test_that("group must be a single existing column name", {
  expect_error(lv_tbl(lv, vars = "nausea", group = c("arm", "nausea")), "single column name")
  expect_error(lv_tbl(lv, vars = "nausea", group = 1), "single column name")
  expect_error(lv_tbl(lv, vars = "nausea", group = "foo"), "'foo'")
})

test_that("group given in vars is removed with a message", {
  expect_message(lv_tbl(lv, vars = c("nausea", "arm"), group = "arm"),
                 "removed from 'vars'")
})


# p-values -------------------------------------------------------------------------

test_that("p-value per variable equals Fisher's exact test", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm", test = TRUE)
  tb  <- tbl$table_body
  for (v in c("nausea", "fatigue")) {
    x <- as.numeric(tolower(as.character(lv[[v]])) %in% c("1", "yes"))
    ref <- stats::fisher.test(table(factor(x, levels = 0:1), lv$arm))$p.value
    expect_equal(as.numeric(tb$p.value[tb$variable == v][1]), ref, tolerance = 1e-8,
                 label = v)
  }
})


# Layout ----------------------------------------------------------------------------

test_that("variables are shown in the order given", {
  tbl <- lv_tbl(lv, vars = c("fatigue", "nausea"))
  expect_equal(tbl$table_body$variable, c("fatigue", "nausea"))
})

test_that("one row per variable", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), group = "arm")
  expect_equal(nrow(tbl$table_body), 2)
})

test_that("`label` is used as column header", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"), label = "Symptoms")
  hdr <- tbl$table_styling$header
  expect_match(hdr$label[hdr$column == "label"], "Symptoms", fixed = TRUE)
})

test_that("footnote says that more than one entry is possible", {
  tbl <- lv_tbl(lv, vars = c("nausea", "fatigue"))
  expect_true(any(grepl("More than one entry possible", unlist(tbl$table_styling),
                        fixed = TRUE)))
})

test_that("no N footnote when add_n = FALSE", {
  tbl <- lv_tbl(lv, vars = "nausea", group = "arm", add_n = FALSE)
  expect_false(any(grepl("counted as absent", unlist(tbl$table_styling), fixed = TRUE)))
})


# Output ----------------------------------------------------------------------------

test_that("return class depends on as_flex_table", {
  expect_s3_class(summaryLevels(lv, vars = "nausea"), "flextable")
  expect_s3_class(summaryLevels(lv, vars = "nausea", border = FALSE), "flextable")
  expect_s3_class(lv_tbl(lv, vars = "nausea"), "gtsummary")
})

test_that("Word file is written", {
  f <- tempfile(fileext = ".docx")
  expect_message(summaryLevels(lv, vars = "nausea", word_output = TRUE, file_path = f),
                 "Table saved to")
  expect_true(file.exists(f))
})
