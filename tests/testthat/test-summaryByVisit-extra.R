# Additional tests for summaryByVisit(): visit ordering, defaults and
# input checks

# Data and helpers -------------------------------------------------------------

# 24 patients x 3 visits, rows shuffled
sv_data <- function(visits = c("Visit 1", "Visit 2", "Visit 10")) {
  set.seed(42)
  d <- expand.grid(id = sprintf("P%02d", 1:24), visit = visits,
                   stringsAsFactors = FALSE)
  d$arm    <- ifelse(as.integer(sub("P", "", d$id)) %% 2 == 0, "A", "B")
  d$weight <- round(rnorm(nrow(d), 70, 10), 1)
  d$ecog   <- sample(c("0", "1", "2"), nrow(d), replace = TRUE)
  d$order  <- match(d$visit, visits)
  d$period <- ifelse(d$order == 1, "Baseline", "Follow-up")
  d[sample(nrow(d)), ]
}

sv <- function(...) summaryByVisit(..., as_flex_table = FALSE)

# Visit labels in the order they appear in the table (first variable only)
sv_visits <- function(tbl, visits) {
  lab <- tbl$table_body$label
  lab <- lab[lab %in% visits]
  lab[!duplicated(lab)]
}

sv_hidden <- function(tbl, col) {
  h <- tbl$table_styling$header
  isTRUE(h$hide[h$column == col])
}

# Level rows of a categorical variable for one visit
sv_cat_cells <- function(tbl, visit, col = "stat_0") {
  tb <- tbl$table_body
  vi <- which(tb$label == visit)[1]
  nx <- which(seq_len(nrow(tb)) > vi & tb$label %in% c("0", "1", "2"))
  nx <- nx[nx <= vi + 3]
  tb[[col]][nx]
}


# Visit ordering -------------------------------------------------------------------

test_that("visits are sorted naturally regardless of the row order", {
  for (s in 1:3) {
    d <- sv_data()
    set.seed(s)
    d <- d[sample(nrow(d)), ]
    tbl <- sv(d, vars = "weight")
    expect_equal(sv_visits(tbl, c("Visit 1", "Visit 2", "Visit 10")),
                 c("Visit 1", "Visit 2", "Visit 10"))
  }
})

test_that("numeric visits are sorted numerically", {
  d <- sv_data()
  d$visit <- as.numeric(sub("Visit ", "", d$visit))
  tbl <- sv(d, vars = "weight")
  expect_equal(sv_visits(tbl, c("1", "2", "10")), c("1", "2", "10"))
})

test_that("visits with several numbers are sorted by each number (cycle, then day)", {
  v <- c("Cycle 2 Day 1", "Cycle 1 Day 15", "Cycle 1 Day 1")
  tbl <- sv(sv_data(v), vars = "weight")
  expect_equal(sv_visits(tbl, v), c("Cycle 1 Day 1", "Cycle 1 Day 15", "Cycle 2 Day 1"))
})

test_that("a factor visit keeps its level order", {
  d <- sv_data()
  d$visit <- factor(d$visit, levels = c("Visit 10", "Visit 2", "Visit 1"))
  tbl <- sv(d, vars = "weight")
  expect_equal(sv_visits(tbl, levels(d$visit)), c("Visit 10", "Visit 2", "Visit 1"))
})

test_that("`order` overrides the natural order", {
  d <- sv_data()
  d$order <- c("Visit 1" = 3, "Visit 2" = 2, "Visit 10" = 1)[d$visit]
  tbl <- sv(d, vars = "weight", order = "order")
  expect_equal(sv_visits(tbl, c("Visit 1", "Visit 2", "Visit 10")),
               c("Visit 10", "Visit 2", "Visit 1"))
})

test_that("visit ordering is the same for continuous and categorical variables", {
  tbl <- sv(sv_data(), vars = c("weight", "ecog"))
  lab <- tbl$table_body$label
  lab <- lab[lab %in% c("Visit 1", "Visit 2", "Visit 10")]
  expect_equal(lab, rep(c("Visit 1", "Visit 2", "Visit 10"), 2))
})

test_that("visit ordering works with a group", {
  tbl <- sv(sv_data(), vars = "weight", group = "arm")
  expect_equal(sv_visits(tbl, c("Visit 1", "Visit 2", "Visit 10")),
               c("Visit 1", "Visit 2", "Visit 10"))
})

test_that("the hemoglobin_data example shows Cycle 1 to Cycle 9 in order", {
  data("hemoglobin_data", package = "summarySCI", envir = environment())
  tbl <- sv(hemoglobin_data, vars = "hb", visit = "visit")
  expect_equal(sv_visits(tbl, paste("Cycle", 1:9)), paste("Cycle", 1:9))
})


# Defaults --------------------------------------------------------------------------

test_that("default: percentages without decimals (digits_cat = 0)", {
  tbl   <- sv(sv_data(), vars = "ecog")
  cells <- sv_cat_cells(tbl, "Visit 1")
  expect_true(length(cells) > 0)
  expect_true(all(grepl("^[0-9]+ \\([0-9]+%\\)$", cells)))
})

test_that("digits_cat = 1 still gives one decimal", {
  tbl   <- sv(sv_data(), vars = "ecog", digits_cat = 1)
  cells <- sv_cat_cells(tbl, "Visit 1")
  expect_true(all(grepl("^[0-9]+ \\([0-9]+\\.[0-9]%\\)$", cells)))   # n without decimals
})

test_that("stat_cat = 'n_N' shows whole numbers, also with digits_cat = 1", {
  tbl   <- sv(sv_data(), vars = "ecog", stat_cat = "n_N", digits_cat = 1)
  cells <- sv_cat_cells(tbl, "Visit 1")
  expect_true(all(grepl("^[0-9]+/[0-9]+$", cells)))
})

test_that("default: N column is shown (add_n = TRUE)", {
  expect_false(sv_hidden(sv(sv_data(), vars = "weight"), "n"))
  tbl <- sv(sv_data(), vars = "weight", group = "arm")
  expect_false(sv_hidden(tbl, "add_n_stat_1"))
  expect_false(sv_hidden(tbl, "add_n_stat_2"))
})

test_that("default: no Overall column with a group (overall = FALSE)", {
  expect_true(sv_hidden(sv(sv_data(), vars = "weight", group = "arm"), "stat_0"))
})

test_that("default N equals the number of patients per visit", {
  tbl <- sv(sv_data(), vars = "weight")
  tb  <- tbl$table_body
  expect_equal(as.numeric(tb$n[tb$label == "Visit 2"][1]), 24)
})


# Labels ------------------------------------------------------------------------------

test_that("unnamed labels are used in the order of vars", {
  tbl <- sv(sv_data(), vars = c("weight", "ecog"), labels = list("Weight (kg)", "ECOG"))
  lab <- tbl$table_body$label
  expect_true(all(c("Weight (kg)", "ECOG") %in% lab))
  expect_true(which(lab == "Weight (kg)") < which(lab == "ECOG"))
})

test_that("labels for only some vars: the others keep their default label", {
  d <- sv_data()
  attr(d$weight, "label") <- "Body weight"
  tbl <- sv(d, vars = c("weight", "ecog"), labels = list(ecog = "ECOG"))
  expect_true(all(c("Body weight", "ECOG") %in% tbl$table_body$label))
})

test_that("labels as a character vector give a clear error", {
  expect_error(sv(sv_data(), vars = "weight", labels = c(weight = "Weight")),
               "must be a list")
})

test_that("named labels are matched to vars in any order", {
  tbl <- sv(sv_data(), vars = c("weight", "ecog"),
            labels = list(ecog = "ECOG", weight = "Weight (kg)"))
  lab <- tbl$table_body$label
  expect_true(which(lab == "Weight (kg)") < which(lab == "ECOG"))
})

test_that("formula labels give a clear error", {
  expect_error(sv(sv_data(), vars = "weight", labels = list(weight ~ "Weight")),
               "formula")
})

test_that("labels must be single strings", {
  expect_error(sv(sv_data(), vars = "weight", labels = list(weight = c("a", "b"))),
               "single, non-missing")
})


# Input checks ----------------------------------------------------------------------------

test_that("data must be a non-empty data frame", {
  expect_error(sv(list(a = 1), vars = "a"), "data frame")
  expect_error(sv(sv_data()[0, ], vars = "weight"), "0 rows")
})

test_that("group, visit, order and visitgroup cannot be used twice", {
  d <- sv_data()
  expect_error(sv(d, vars = "weight", group = "visit"), "different columns")
  expect_error(sv(d, vars = c("weight", "arm"), group = "arm"), "cannot also be in `vars`")
  expect_error(sv(d, vars = c("weight", "visit")), "cannot also be in `vars`")
  expect_error(sv(d, vars = c("weight", "order"), order = "order"), "cannot also be in `vars`")
  expect_error(sv(d, vars = "weight", group = "arm", visitgroup = "arm"), "different columns")
})

test_that("duplicated vars give an error", {
  expect_error(sv(sv_data(), vars = c("weight", "weight")), "duplicates")
})

test_that("group with one level gives an error, missing group values a message", {
  d <- sv_data()
  d$one <- "A"
  expect_error(sv(d, vars = "weight", group = "one"), "At least 2 groups")
  d$arm[1:3] <- NA
  expect_message(sv(d, vars = "weight", group = "arm"), "missing value")
})

test_that("order must be numeric and unique per visit", {
  d <- sv_data()
  d$order_chr <- as.character(d$order)
  expect_error(sv(d, vars = "weight", order = "order_chr"), "must be numeric")
  d$order_bad <- d$order
  d$order_bad[1] <- 99
  expect_error(sv(d, vars = "weight", order = "order_bad"), "exactly one value per visit")
})

test_that("visitgroup must be constant within each visit", {
  d <- sv_data()
  d$period[d$visit == "Visit 2"][1] <- "Baseline"
  expect_error(sv(d, vars = "weight", visitgroup = "period"), "constant within each visit")
})

test_that("invalid options give clear errors", {
  d <- sv_data()
  expect_error(sv(d, vars = "weight", digits_cont = -1), "digits_cont")
  expect_error(sv(d, vars = "ecog", digits_cat = 1.5), "digits_cat")
  expect_error(sv(d, vars = "weight", missing = "yes"), "TRUE or FALSE")
  expect_error(sv(d, vars = "ecog", missing_percent = "all"), "missing_percent")
  expect_error(sv(d, vars = "weight", add_n = "yes"), "TRUE or FALSE")
  expect_error(sv(d, vars = "weight", overall = NA), "TRUE or FALSE")
  expect_error(sv(d, vars = "weight", continuous_as_categorical = "foo"), "not in `data`")
})

test_that("Word output: file_path must end with .docx and its folder must exist", {
  d <- sv_data()
  expect_error(summaryByVisit(d, vars = "weight", word_output = TRUE,
                              file_path = file.path(tempdir(), "table.txt")), ".docx")
  expect_error(summaryByVisit(d, vars = "weight", word_output = TRUE,
                              file_path = file.path(tempdir(), "no_such_folder", "t.docx")),
               "does not exist")
})
