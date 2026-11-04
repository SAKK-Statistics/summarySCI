# Tests for the internal helper functions in R/helpers.R

# fisher_test_safe() ---------------------------------------------------------

# Table that makes stats::fisher.test() fail with the default workspace
# (FEXACT error 7), like `metastases_total` in INVINCIBLE-4
fexact_data <- function() {
  set.seed(1)
  d <- data.frame(arm = sample(c("A", "B"), 400, TRUE),
                  met = rpois(400, 3) + rbinom(400, 12, 0.1))
  d$met[sample(400, 20)] <- NA
  d
}

# Harder table (3 groups, many levels): needs the Monte Carlo fallback
mc_data <- function() {
  set.seed(3)
  data.frame(arm = sample(c("A", "B", "C"), 600, TRUE), v = rpois(600, 6))
}

test_that("fisher_test_safe returns a data frame with p.value and method", {
  d <- data.frame(x = c("a", "b", "a", "b", "a", "a"), g = c(1, 1, 1, 2, 2, 2))
  res <- fisher_test_safe(d, "x", "g")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("p.value", "method"))
  expect_equal(nrow(res), 1)
})

test_that("fisher_test_safe gives the same p-value as fisher.test for small tables", {
  d <- data.frame(x = c("a", "b", "a", "b", "a", "a", "b", "b"),
                  g = c(1, 1, 1, 1, 2, 2, 2, 2))
  ref <- stats::fisher.test(table(d$x, d$g))$p.value
  expect_equal(fisher_test_safe(d, "x", "g")$p.value, ref)
  expect_equal(fisher_test_safe(d, "x", "g")$method, "Fisher's exact test")
})

test_that("plain fisher.test fails on the FEXACT data (the original problem)", {
  d <- fexact_data()
  expect_error(stats::fisher.test(table(d$met, d$arm)), "FEXACT")
})

test_that("fisher_test_safe does not fail on the FEXACT data and keeps an exact p-value", {
  d <- fexact_data()
  expect_no_error(res <- fisher_test_safe(d, "met", "arm"))
  expect_true(res$p.value >= 0 && res$p.value <= 1)
  expect_equal(res$method, "Fisher's exact test")
})

test_that("fisher_test_safe falls back to a Monte Carlo p-value when needed", {
  skip_on_cran()
  d <- mc_data()
  res <- fisher_test_safe(d, "v", "arm")
  expect_true(res$p.value >= 0 && res$p.value <= 1)
  expect_match(res$method, "Monte Carlo")
  expect_match(res$method, "100,000", fixed = TRUE)
})

test_that("Monte Carlo p-value is reproducible", {
  skip_on_cran()
  d <- mc_data()
  expect_identical(fisher_test_safe(d, "v", "arm"), fisher_test_safe(d, "v", "arm"))
})

test_that("fisher_test_safe does not change the user's random number stream", {
  skip_on_cran()
  d <- mc_data()
  set.seed(123); a <- runif(3)
  set.seed(123); invisible(fisher_test_safe(d, "v", "arm")); b <- runif(3)
  expect_identical(a, b)
})

test_that("fisher_test_safe does not create a seed if there was none", {
  skip_on_cran()
  d <- mc_data()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    rm(".Random.seed", envir = globalenv())
  }
  invisible(fisher_test_safe(d, "v", "arm"))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("fisher_test_safe returns NA for degenerate tables", {
  one_level <- data.frame(x = c("a", "a", "a", "a"), g = c(1, 1, 2, 2))
  one_group <- data.frame(x = c("a", "b", "a", "b"), g = c(1, 1, 1, 1))
  all_na    <- data.frame(x = c(NA, NA, NA, NA), g = c(1, 1, 2, 2))
  expect_true(is.na(fisher_test_safe(one_level, "x", "g")$p.value))
  expect_true(is.na(fisher_test_safe(one_group, "x", "g")$p.value))
  expect_true(is.na(fisher_test_safe(all_na, "x", "g")$p.value))
})

test_that("fisher_test_safe ignores missing values and empty factor levels", {
  d <- data.frame(x = factor(c("a", "b", "a", NA, "b", "a"), levels = c("a", "b", "c")),
                  g = c(1, 1, 1, 2, 2, NA))
  dd  <- d[!is.na(d$x) & !is.na(d$g), ]
  ref <- stats::fisher.test(table(droplevels(dd$x), dd$g))$p.value
  expect_equal(fisher_test_safe(d, "x", "g")$p.value, ref)
})


# natural_levels() / as_natural_factor(): additional cases --------------------

test_that("natural_levels handles numeric input", {
  expect_equal(natural_levels(c(10, 2, 1, 2)), c("1", "2", "10"))
})

test_that("natural_levels handles decimals and negative numbers", {
  expect_equal(natural_levels(c("1.5", "-2", "0.5", "10")), c("-2", "0.5", "1.5", "10"))
})

test_that("natural_levels treats a decimal comma as a decimal point", {
  expect_equal(natural_levels(c("1,5", "1,25", "2")), c("1,25", "1,5", "2"))
})

test_that("natural_levels removes duplicates and NA", {
  expect_equal(natural_levels(c("3", NA, "3", "1", NA)), c("1", "3"))
  expect_equal(natural_levels(c(NA, NA)), character(0))
})

test_that("natural_levels ignores leading and trailing spaces for the ordering", {
  expect_equal(natural_levels(c(" 10", "2 ")), c("2 ", " 10"))
})

test_that("natural_levels orders several numbers in a text value (cycle and day)", {
  x <- c("Cycle 2 Day 1", "Cycle 1 Day 15", "Cycle 1 Day 1", "Cycle 10 Day 1")
  expect_equal(natural_levels(x),
               c("Cycle 1 Day 1", "Cycle 1 Day 15", "Cycle 2 Day 1", "Cycle 10 Day 1"))
})

test_that("natural_levels puts operators right around their number", {
  expect_equal(natural_levels(c(">2", "2", "<2", "1", "3")), c("1", "<2", "2", ">2", "3"))
  expect_equal(natural_levels(c(">=2", "<=2", "2")), c("<=2", "2", ">=2"))
})

test_that("natural_levels: '<' alone or with text is not a number", {
  expect_equal(natural_levels(c("< LLN", "1", "> ULN")), c("1", "< LLN", "> ULN"))
})

test_that("natural_levels is stable for values that differ only in text", {
  expect_equal(natural_levels(c("10 b", "10 a")), c("10 a", "10 b"))
})

test_that("as_natural_factor returns a factor with all values and the label", {
  x <- c("10", "3", NA, "3")
  attr(x, "label") <- "Count"
  f <- as_natural_factor(x)
  expect_s3_class(f, "factor")
  expect_equal(levels(f), c("3", "10"))
  expect_equal(as.character(f), c("10", "3", NA, "3"))
  expect_equal(attr(f, "label"), "Count")
})

test_that("as_natural_factor works without a label", {
  f <- as_natural_factor(c("b", "a"))
  expect_null(attr(f, "label"))
  expect_equal(levels(f), c("a", "b"))
})


# get_labels() ------------------------------------------------------------------

test_that("get_labels uses the label attribute, else the variable name", {
  d <- data.frame(a = 1:2, b = 3:4)
  attr(d$a, "label") <- "Age (years)"
  expect_equal(get_labels(d, c("a", "b")), list(a = "Age (years)", b = "b"))
})

test_that("get_labels ignores empty labels and unknown columns", {
  d <- data.frame(a = 1:2)
  attr(d$a, "label") <- ""
  expect_equal(get_labels(d, c("a", "zz")), list(a = "a", zz = "zz"))
})


# geom_mean() / se() -------------------------------------------------------------

test_that("geom_mean is correct and ignores NA", {
  expect_equal(geom_mean(c(1, 10, 100)), 10)
  expect_equal(geom_mean(c(2, 8, NA)), 4)
})

test_that("se is sd / sqrt(n)", {
  x <- c(2, 4, 4, 4, 5, 5, 7, 9)
  expect_equal(se(x), stats::sd(x) / sqrt(length(x)))
})


# is_dichotomous() ---------------------------------------------------------------

test_that("is_dichotomous recognises 0/1, yes/no and logical", {
  expect_true(is_dichotomous(c(0, 1, 1, NA)))
  expect_true(is_dichotomous(c("yes", "No", NA)))
  expect_true(is_dichotomous(factor(c("Yes", "no"))))
  expect_true(is_dichotomous(c(TRUE, FALSE)))
})

test_that("is_dichotomous is FALSE for other variables", {
  expect_false(is_dichotomous(c(0, 1, 2)))
  expect_false(is_dichotomous(c(1, 2)))
  expect_false(is_dichotomous(c("a", "b")))
  expect_false(is_dichotomous(c(1, 1, 1)))
})


# check_no_dates() ---------------------------------------------------------------

test_that("check_no_dates stops for Date, POSIXct and difftime variables", {
  d <- data.frame(a = 1:2,
                  dt = as.Date(c("2026-01-01", "2026-02-01")),
                  tm = as.POSIXct(c("2026-01-01 10:00", "2026-01-01 11:00"), tz = "UTC"))
  d$dur <- d$dt - as.Date("2025-12-31")
  expect_error(check_no_dates(d, "dt"), "`dt`")
  expect_error(check_no_dates(d, "tm"), "`tm`")
  expect_error(check_no_dates(d, "dur"), "`dur`")
  expect_error(check_no_dates(d, "a", group = "dt"), "`group`")
})

test_that("check_no_dates passes for other variables", {
  d <- data.frame(a = 1:2, b = c("x", "y"))
  expect_true(check_no_dates(d, c("a", "b")))
})


# format lookups -----------------------------------------------------------------

test_that("cat_digits: no decimals for counts, digits_cat for percentages", {
  expect_equal(cat_digits("{n} ({p}%)", 1), c(0, 1))
  expect_equal(cat_digits("{n}", 2), 0)
  expect_equal(cat_digits("{n}/{N}", 1), c(0, 0))
  expect_equal(cat_digits(format_lookup_cat$n_percent, 0), c(0, 0))
})

test_that("all documented statistics exist in the lookups", {
  expect_setequal(names(format_lookup),
                  c("mean_sd", "mean_se", "median_range", "median_IQR", "geomMean_sd"))
  expect_setequal(names(format_lookup_cat), c("n_percent", "n", "n_N"))
})
