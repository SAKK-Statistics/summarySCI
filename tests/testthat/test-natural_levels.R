# Natural ordering of categories -------------------------------------------

test_that("numbers are sorted numerically, text comes last", {
  expect_equal(
    natural_levels(c("10", "3", "more than 10", "9", NA)),
    c("3", "9", "10", "more than 10")
  )
})

test_that("leading comparison operators are taken into account", {
  expect_equal(
    natural_levels(c(">90%", "20%", "< 10%")),
    c("< 10%", "20%", ">90%")
  )
  expect_equal(
    natural_levels(c(">90%", "10-20%", "<10%", "20-90%")),
    c("<10%", "10-20%", "20-90%", ">90%")
  )
  expect_equal(
    natural_levels(c(">= 5", "5", "<5", "<= 5", ">5")),
    c("<5", "<= 5", "5", ">= 5", ">5")
  )
  expect_equal(
    natural_levels(c("≥ 65", "65", "< 65", "≤ 64")),
    c("≤ 64", "< 65", "65", "≥ 65")
  )
  expect_equal(
    natural_levels(c("unknown", ">= 65", "18-64", "< 18")),
    c("< 18", "18-64", ">= 65", "unknown")
  )
})

test_that("numbers inside text and plain text are sorted sensibly", {
  expect_equal(natural_levels(c("Arm 10", "Arm 2", "Arm 1")),
               c("Arm 1", "Arm 2", "Arm 10"))
  expect_equal(natural_levels(c("Male", "Female")), c("Female", "Male"))
  expect_equal(natural_levels(c("10 doses", "none", "1 dose", "2 doses")),
               c("1 dose", "2 doses", "10 doses", "none"))
  expect_equal(natural_levels(character(0)), character(0))
})

test_that("as_natural_factor keeps the label", {
  x <- c(">90%", "<10%", "20%", NA)
  attr(x, "label") <- "Percentage"
  f <- as_natural_factor(x)
  expect_s3_class(f, "factor")
  expect_equal(levels(f), c("<10%", "20%", ">90%"))
  expect_equal(attr(f, "label"), "Percentage")
  expect_true(is.na(f[4]))
})

# Ordering in summaryTable() ------------------------------------------------

get_levels <- function(tbl, var) {
  tbl$table_body$label[tbl$table_body$variable == var &
                         tbl$table_body$row_type == "level"]
}

test_that("summaryTable shows character categories in natural order", {
  dat <- data.frame(
    mets  = c("10", "3", "more than 10", "9", "3", "10", "4", "more than 10"),
    pct   = c(">90%", "< 10%", "20%", "< 10%", ">90%", "20%", "20%", "< 10%"),
    group = rep(c("A", "B"), 4)
  )

  tbl <- summaryTable(dat, vars = c("mets", "pct"), group = "group",
                      as_flex_table = FALSE)

  expect_equal(get_levels(tbl, "mets"), c("3", "4", "9", "10", "more than 10"))
  expect_equal(get_levels(tbl, "pct"), c("< 10%", "20%", ">90%"))
})

test_that("existing factor levels are not reordered", {
  dat <- data.frame(
    pct   = factor(c(">90%", "< 10%", "20%", ">90%"),
                   levels = c(">90%", "20%", "< 10%")),
    group = c("A", "B", "A", "B")
  )

  tbl <- summaryTable(dat, vars = "pct", group = "group", as_flex_table = FALSE)

  expect_equal(get_levels(tbl, "pct"), c(">90%", "20%", "< 10%"))
})

test_that("a missing row stays at the end", {
  dat <- data.frame(
    mets  = c("10", "3", NA, "more than 10", "3", "10"),
    group = c("A", "B", "A", "B", "A", "B")
  )

  tbl <- summaryTable(dat, vars = "mets", group = "group", as_flex_table = FALSE)

  expect_equal(get_levels(tbl, "mets"), c("3", "10", "more than 10", "Missing"))
})
