# Additional tests for summaryTable(): p-values for variables with many
# categories, natural ordering, defaults and input checks

# Data and helpers -------------------------------------------------------------

# Variable with many categories: plain fisher.test() fails (FEXACT error 7)
st_fexact <- function() {
  set.seed(1)
  d <- data.frame(arm = sample(c("A", "B"), 400, TRUE),
                  met = rpois(400, 3) + rbinom(400, 12, 0.1))
  d$met[sample(400, 20)] <- NA
  d$met_cat <- as.character(d$met)
  d
}

# 3 groups, many categories: needs the Monte Carlo p-value
st_mc <- function() {
  set.seed(3)
  d <- data.frame(arm = sample(c("A", "B", "C"), 600, TRUE), v = rpois(600, 6))
  d$v_cat <- as.character(d$v)
  d
}

st_small <- function() {
  data.frame(
    age   = c(55, 61, 47, 70, 66, 58, 62, 49, 73, 51),
    sex   = c("F", "M", "F", "M", "F", "F", "M", "M", "F", "M"),
    stage = c("II", "III", "II", "IV", "III", "II", "IV", "III", "II", "II"),
    arm   = rep(c("Arm 10", "Arm 2"), 5)
  )
}

st <- function(...) summaryTable(..., as_flex_table = FALSE)

st_visible_cols <- function(tbl) {
  hdr <- tbl$table_styling$header
  names(tbl$table_body)[names(tbl$table_body) %in% hdr$column[!hdr$hide]]
}

st_header <- function(tbl, pattern) {
  hdr <- tbl$table_styling$header
  hdr$label[grep(pattern, hdr$column)[1]]
}

st_p <- function(tbl, var) {
  tb   <- tbl$table_body
  pcol <- grep("^p.value", names(tb), value = TRUE)[1]
  if (is.na(pcol)) return(NULL)
  unname(as.numeric(tb[[pcol]][tb$variable == var & tb$row_type == "label"]))
}

st_levels <- function(tbl, var) {
  tb <- tbl$table_body
  tb$label[tb$variable == var & tb$row_type == "level"]
}


# P-values for variables with many categories -----------------------------------

test_that("no Fisher error for a variable with many categories when test = FALSE", {
  d <- st_fexact()
  expect_no_error(tbl <- st(d, vars = "met_cat", group = "arm"))
  expect_length(grep("^p.value", names(tbl$table_body)), 0)
})

test_that("no Fisher error for many categories with overall, add_n and missing", {
  d <- st_fexact()
  expect_no_error(st(d, vars = "met_cat", group = "arm", overall = TRUE,
                     add_n = TRUE, missing = TRUE))
})

test_that("test = TRUE gives the exact Fisher p-value for many categories", {
  d   <- st_fexact()
  tbl <- st(d, vars = "met_cat", group = "arm", test = TRUE)
  dd  <- d[!is.na(d$met_cat), ]
  ref <- stats::fisher.test(table(dd$met_cat, dd$arm), workspace = 2e7)$p.value
  expect_equal(st_p(tbl, "met_cat"), ref, tolerance = 1e-8)
})

test_that("test = TRUE with 3 groups and many categories gives a Monte Carlo p-value", {
  d   <- st_mc()
  expect_no_error(tbl <- st(d, vars = "v_cat", group = "arm", test = TRUE))
  p <- st_p(tbl, "v_cat")
  expect_true(p >= 0 && p <= 1)
  expect_true(any(grepl("Monte Carlo", unlist(tbl$table_styling), fixed = TRUE)))
})

test_that("Monte Carlo p-value in summaryTable is reproducible", {
  d <- st_mc()
  expect_equal(st_p(st(d, vars = "v_cat", group = "arm", test = TRUE), "v_cat"),
               st_p(st(d, vars = "v_cat", group = "arm", test = TRUE), "v_cat"))
})

test_that("normal Fisher p-values are unchanged by the safe wrapper", {
  d   <- st_small()
  tbl <- st(d, vars = "stage", group = "sex", test = TRUE)
  ref <- stats::fisher.test(table(d$stage, d$sex))$p.value
  expect_equal(st_p(tbl, "stage"), ref, tolerance = 1e-8)
})

test_that("other categorical tests are not replaced by the Fisher wrapper", {
  d   <- st_small()
  tbl <- st(d, vars = "stage", group = "sex", test = TRUE, test_cat = "chisq.test")
  ref <- suppressWarnings(stats::chisq.test(table(d$stage, d$sex))$p.value)
  expect_equal(st_p(tbl, "stage"), ref, tolerance = 1e-8)
})


# Natural ordering of groups ------------------------------------------------------

test_that("a character group is ordered naturally ('Arm 2' before 'Arm 10')", {
  tbl <- st(st_small(), vars = "age", group = "arm")
  expect_match(st_header(tbl, "^stat_1"), "Arm 2", fixed = TRUE)
  expect_match(st_header(tbl, "^stat_2"), "Arm 10", fixed = TRUE)
})

test_that("a numeric-text group is ordered numerically", {
  d <- st_small()
  d$cycles <- rep(c("10", "2"), 5)
  tbl <- st(d, vars = "age", group = "cycles")
  expect_match(st_header(tbl, "^stat_1"), "2", fixed = TRUE)
  expect_match(st_header(tbl, "^stat_2"), "10", fixed = TRUE)
})

test_that("a factor group keeps its level order", {
  d <- st_small()
  d$arm <- factor(d$arm, levels = c("Arm 10", "Arm 2"))
  tbl <- st(d, vars = "age", group = "arm")
  expect_match(st_header(tbl, "^stat_1"), "Arm 10", fixed = TRUE)
})

test_that("natural order is the same with overall, test and missing_percent = 'both'", {
  d <- st_small()
  d$n_les <- c("10", "3", "more than 10", "3", "10", "3", "10", NA, "3", "more than 10")
  for (args in list(list(), list(overall = TRUE), list(test = TRUE),
                    list(missing_percent = "both"))) {
    tbl <- do.call(st, c(list(d, vars = "n_les", group = "sex"), args))
    lv  <- unique(st_levels(tbl, "n_les"))
    lv  <- lv[lv != "Missing"]
    expect_equal(lv, c("3", "10", "more than 10"))
  }
})

test_that("character variable keeps its label after natural ordering", {
  d <- st_small()
  attr(d$stage, "label") <- "Tumour stage"
  tbl <- st(d, vars = "stage")
  tb  <- tbl$table_body
  expect_equal(tb$label[tb$variable == "stage" & tb$row_type == "label"][1], "Tumour stage")
})

test_that("Roman numerals are not reordered numerically (stay alphabetical)", {
  tbl <- st(st_small(), vars = "stage")
  expect_equal(st_levels(tbl, "stage"), c("II", "III", "IV"))
})


# Defaults -----------------------------------------------------------------------

test_that("default: percentages without decimals (digits_cat = 0)", {
  tbl   <- st(st_small(), vars = "stage", group = "sex")
  cells <- tbl$table_body$stat_1[tbl$table_body$row_type == "level"]
  expect_true(all(grepl("^[0-9]+ \\([0-9]+%\\)$", cells)))
})

test_that("digits_cat = 1: one decimal for %, none for n", {
  tbl   <- st(st_small(), vars = "stage", group = "sex", digits_cat = 1)
  cells <- tbl$table_body$stat_1[tbl$table_body$row_type == "level"]
  expect_true(all(grepl("^[0-9]+ \\([0-9]+\\.[0-9]%\\)$", cells)))
})

test_that("stat_cat = 'n_N' shows whole numbers, also with digits_cat = 1", {
  tbl   <- st(st_small(), vars = "stage", group = "sex", stat_cat = "n_N", digits_cat = 1)
  cells <- tbl$table_body$stat_1[tbl$table_body$row_type == "level"]
  expect_true(all(grepl("^[0-9]+/[0-9]+$", cells)))
})

test_that("default: N columns are shown (add_n = TRUE)", {
  tbl <- st(st_small(), vars = "age", group = "sex")
  expect_true(any(grepl("^add_n_stat_1", st_visible_cols(tbl))))
})

test_that("default: no Overall column (overall = FALSE)", {
  tbl <- st(st_small(), vars = "age", group = "sex")
  expect_length(grep("^stat_0", st_visible_cols(tbl)), 0)
})

test_that("add_n = FALSE hides all N columns", {
  tbl <- st(st_small(), vars = "age", group = "sex", overall = TRUE, add_n = FALSE)
  expect_length(grep("^(add_n_stat|n$|n_)", st_visible_cols(tbl)), 0)
})

test_that("vars = NULL summarises all variables except group", {
  tbl <- st(st_small(), group = "arm")
  expect_setequal(unique(tbl$table_body$variable), c("age", "sex", "stage"))
})


# Confidence intervals -------------------------------------------------------------

st_ci <- function(tbl) {
  tb <- tbl$table_body
  x  <- unlist(tb[grepl("^ci_stat", names(tb))])
  x[!is.na(x) & nzchar(x)]
}

test_that("both spellings of Clopper-Pearson give the same CI", {
  d <- st_small()
  expect_equal(st_ci(st(d, vars = "sex", ci = TRUE, ci_cat = "clopper.pearson")),
               st_ci(st(d, vars = "sex", ci = TRUE, ci_cat = "clopper-pearson")))
})

test_that("Clopper-Pearson is the default CI", {
  d <- st_small()
  expect_equal(st_ci(st(d, vars = "sex", ci = TRUE)),
               st_ci(st(d, vars = "sex", ci = TRUE, ci_cat = "clopper.pearson")))
})

test_that("an unknown CI method gives an error", {
  expect_error(st(st_small(), vars = "sex", ci = TRUE, ci_cat = "foo"), "does not exist")
})


# Input checks and messages ------------------------------------------------------------

test_that("group given in vars is removed with a message", {
  expect_message(st(st_small(), vars = c("age", "arm"), group = "arm"),
                 "removed from 'vars'")
})

test_that("a variable with only NA is dropped with a message", {
  d <- st_small()
  d$empty <- NA_real_
  expect_message(tbl <- st(d, vars = c("age", "empty")), "only contains NA")
  expect_false("empty" %in% tbl$table_body$variable)
})

test_that("empty levels of group are dropped with a message", {
  d <- st_small()
  d$sex <- factor(d$sex, levels = c("F", "M", "X"))
  expect_message(st(d, vars = "age", group = "sex"), "Empty level")
})

test_that("group with only one level gives an error", {
  d <- st_small()
  d$one <- "A"
  expect_error(st(d, vars = "age", group = "one"), "at least two levels")
})

test_that("test = TRUE without group gives an error", {
  expect_error(st(st_small(), vars = "age", test = TRUE), "needs to be given")
})

test_that("group must be a single quoted column name", {
  expect_error(st(st_small(), vars = "age", group = c("sex", "arm")), "single column name")
  expect_error(st(st_small(), vars = "age", group = 1), "single column name")
})

test_that("vars must be a character vector", {
  expect_error(st(st_small(), vars = 1:2), "character vector")
})

test_that("unknown variables are named in the error", {
  expect_error(st(st_small(), vars = c("age", "foo", "bar")), "foo, bar")
  expect_error(st(st_small(), vars = "age", group = "foo"), "'foo'")
})


# Output ----------------------------------------------------------------------------

test_that("flextable is returned with and without border", {
  expect_s3_class(summaryTable(st_small(), vars = "age"), "flextable")
  expect_s3_class(summaryTable(st_small(), vars = "age", border = FALSE), "flextable")
})

test_that("default file_path is used when word_output = TRUE", {
  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)
  f <- paste0("SummaryTable_", format(Sys.Date(), "%Y%m%d"), ".docx")
  on.exit(unlink(f), add = TRUE)
  expect_message(summaryTable(st_small(), vars = "age", word_output = TRUE), "Table saved to")
  expect_true(file.exists(f))
})
