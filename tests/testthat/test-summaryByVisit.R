
# test data for summaryByVisit
data<-NULL
visit <- c(paste0(rep("Visit ", 10), rbind(c(10:1))),
           paste0(rep("Visit ", 10), rbind(c(10:1))),
           paste0(rep("Visit ", 10), rbind(c(10:1))))
data <- as.data.frame(cbind( visit, rnorm(30)))
data<-as.data.frame(rbind(data, data, data, data, data))
data$visitgroup<- ifelse(data$visit %in% c("Visit 1", "Visit 2"), "Baseline", ifelse(data$visit %in% c("Visit 3", "Visit 4"), "Treatment", "Follow-up"))
data$visitgroup<-factor(data$visitgroup, levels = c("Baseline", "Treatment", "Follow-up"))
data$LDH<-rnorm(150)
data$LDH[5]<-NA
data$Lymphocytes<-rnorm(150)
data$ANC<-rnorm(150)
attr(data$LDH, "label")<-"LLDDHH"
attr(data$Lymphocytes, "label")<-"LLymphoctyes"
attr(data$ANC, "label")<-"AANC"
data$arm<- c(rep("Arm A", 70), rep("Arm B", 80))
data$arm<- c(rep("Arm A", 50), rep("Arm B", 50), rep("Arm C", 50))


testthat::test_that("Error when no data given", {
  testthat::expect_error(summaryByVisit(data = NULL))
})



testthat::test_that("median and range are correct", {
  tbl <- summaryByVisit(data = data,
                      vars = "LDH",
                      as_flex_table = FALSE,
                      digits_cont = 0)

  summary_ldh <- summary(data$LDH[data$visit=="Visit 1"])
  median_1 <- round(summary_ldh["Median"], 0)
  min_1 <- round(summary_ldh["Min."], 0)
  max_1 <- round(summary_ldh["Max."], 0)
  median_range_fct <- tbl[["table_body"]][["stat_0"]][[2]]
  median_range_truth <- paste0(median_1, " (",min_1, ", ", max_1, ")")
  testthat::expect_equal(median_range_fct,median_range_truth )

})


testthat::test_that("Number of non-missing observations is correct", {
  tbl_5 <- summaryByVisit(data = data,
                        vars = "LDH",
                        visit="visit",
                        as_flex_table = FALSE,
                        add_n = TRUE)

  n_noMissing_fct <- as.numeric(tbl_5[["table_body"]][["n"]][7])
  n_noMissing_truth <- sum(!is.na(data$LDH[visit=="Visit 6"]))
  testthat::expect_equal(n_noMissing_fct, n_noMissing_truth)

})


# ---------------------------------------------------------------------------- #
# Test data and helpers ----
# (can be moved to tests/testthat/helper-summaryByVisit.R)
# ---------------------------------------------------------------------------- #

# Long data: 30 patients x 3 visits, some drop-outs, some missing values,
# rows shuffled so the function has to order the visits itself.
make_visit_data <- function() {
  set.seed(2026)
  n_pat <- 30
  pts <- data.frame(
    id   = sprintf("P%02d", seq_len(n_pat)),
    arm  = factor(sample(c("A", "B"), n_pat, replace = TRUE)),
    arm3 = factor(sample(c("X", "Y", "Z"), n_pat, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  visits <- data.frame(
    visit       = c("Visit 1", "Visit 2", "Visit 3"),
    visit_order = 1:3,
    period      = factor(c("Baseline", "Follow-up", "Follow-up"),
                         levels = c("Baseline", "Follow-up"), ordered = TRUE),
    stringsAsFactors = FALSE
  )
  d <- merge(pts, visits)                                     # cross join
  d <- d[-sample(which(d$visit != "Visit 1"), 8), ]           # drop-outs
  n <- nrow(d)
  d$weight <- round(rnorm(n, 70, 10), 1)
  d$weight[sample(n, 5)] <- NA
  d$crp    <- round(rlnorm(n, 1.5, 0.6), 2)
  d$ecog   <- factor(sample(c("0", "1", "2"), n, replace = TRUE), levels = c("0", "1", "2"))
  d$ecog[sample(n, 6)] <- NA
  d$resp   <- rbinom(n, 1, 0.4)
  d <- d[sample(n), ]                                         # shuffle rows
  rownames(d) <- NULL
  attr(d$weight, "label") <- "Weight (kg)"                    # labels after subsetting
  attr(d$ecog, "label")   <- "ECOG"
  d
}

vd <- make_visit_data()
VISITS <- c("Visit 1", "Visit 2", "Visit 3")

# Row index of `visit` in table_body (first occurrence after the row labelled `after`)
visit_row <- function(tbl, visit, after = NULL) {
  tb    <- tbl[["table_body"]]
  start <- if (is.null(after)) 0 else which(tb$label == after)[1]
  which(tb$label == visit & seq_len(nrow(tb)) > start)[1]
}

# Continuous: the statistic sits on the visit row
cont_cell <- function(tbl, visit, col = "stat_0", after = NULL) {
  tbl[["table_body"]][[col]][visit_row(tbl, visit, after)]
}

# Categorical: the level row directly following the visit row
cat_cell <- function(tbl, visit, level, col = "stat_0", after = NULL) {
  tb <- tbl[["table_body"]]
  vi <- visit_row(tbl, visit, after)
  li <- which(tb$label == level & seq_len(nrow(tb)) > vi)[1]
  tb[[col]][li]
}

# All numbers in a cell, e.g. "70.2 (51.0, 88.3)" -> c(70.2, 51, 88.3)
nums <- function(x) as.numeric(regmatches(x, gregexpr("-?[0-9]+\\.?[0-9]*", x))[[1]])

expect_rounded <- function(cell, ref, digits) {
  got <- nums(cell)
  expect_equal(length(got), length(ref), label = paste0("number of values in '", cell, "'"))
  expect_true(all(abs(got - ref) <= 0.5 * 10^(-digits) + 1e-8),
              label = paste0("'", cell, "' vs reference ", paste(signif(ref, 6), collapse = ", ")))
}

header_label <- function(tbl, col) {
  h <- tbl$table_styling$header
  h$label[h$column == col]
}
is_hidden <- function(tbl, col) {
  h <- tbl$table_styling$header
  isTRUE(h$hide[h$column == col])
}

sbv <- function(...) summaryByVisit(..., as_flex_table = FALSE)


# ---------------------------------------------------------------------------- #
# Basic structure ----
# ---------------------------------------------------------------------------- #

test_that("runs with defaults for one continuous and one categorical variable", {
  expect_no_error(sbv(vd, vars = "weight"))
  expect_no_error(sbv(vd, vars = "ecog"))
  expect_no_error(sbv(vd, vars = c("weight", "ecog"), group = "arm"))
})

test_that("works without file_path when word_output = FALSE", {
  # default is `file_path = file_path` (recursive default) -> must not be evaluated
  expect_no_error(summaryByVisit(vd, vars = "weight"))
})

test_that("return class depends on as_flex_table", {
  expect_s3_class(summaryByVisit(vd, vars = "weight"), "flextable")
  expect_s3_class(sbv(vd, vars = "weight"), "gtsummary")
})

test_that("each visit appears once per variable", {
  tbl <- sbv(vd, vars = c("weight", "ecog"))
  for (v in VISITS) expect_equal(sum(tbl$table_body$label == v), 2, label = v)
})

test_that("visits are in numeric order, not alphabetical (Visit 2 before Visit 10)", {
  d <- vd
  d$visit <- c("Visit 1" = "Visit 1", "Visit 2" = "Visit 2", "Visit 3" = "Visit 10")[d$visit]
  tbl <- sbv(d, vars = "weight")
  lab <- tbl$table_body$label
  expect_true(which(lab == "Visit 2") < which(lab == "Visit 10"))
})

test_that("visit order works when the visit column is not called 'visit'", {
  d <- vd
  d$visit <- c("Visit 1" = "Visit 1", "Visit 2" = "Visit 2", "Visit 3" = "Visit 10")[d$visit]
  names(d)[names(d) == "visit"] <- "timepoint"
  tbl <- sbv(d, vars = "weight", visit = "timepoint")
  lab <- tbl$table_body$label
  expect_true(which(lab == "Visit 1") < which(lab == "Visit 2"))
  expect_true(which(lab == "Visit 2") < which(lab == "Visit 10"))
})

test_that("`order` defines the visit order", {
  d <- vd
  d$visit <- c("Visit 1" = "Screening", "Visit 2" = "Week 4", "Visit 3" = "Week 12")[d$visit]
  tbl <- sbv(d, vars = "weight", order = "visit_order")
  lab <- tbl$table_body$label
  expect_equal(lab[lab %in% c("Screening", "Week 4", "Week 12")],
               c("Screening", "Week 4", "Week 12"))
})

test_that("rows without visit are removed", {
  d <- vd
  d$visit[1:3] <- NA
  d$visit[4]   <- ""
  tbl <- sbv(d, vars = "weight")
  expect_false(any(tbl$table_body$label %in% c("", "NA"), na.rm = TRUE))
})


# ---------------------------------------------------------------------------- #
# Continuous statistics ----
# ---------------------------------------------------------------------------- #

test_that("median (range) per visit is correct (no group)", {
  tbl <- sbv(vd, vars = "weight", stat_cont = "median_range", digits_cont = 1)
  for (v in VISITS) {
    x <- vd$weight[vd$visit == v & !is.na(vd$weight)]
    expect_rounded(cont_cell(tbl, v), c(median(x), min(x), max(x)), 1)
  }
})

test_that("mean (SD), mean (SE), median (IQR) per visit are correct", {
  for (sc in c("mean_sd", "mean_se", "median_IQR")) {
    tbl <- sbv(vd, vars = "weight", stat_cont = sc, digits_cont = 2)
    for (v in VISITS) {
      x <- vd$weight[vd$visit == v & !is.na(vd$weight)]
      ref <- switch(sc,
                    mean_sd    = c(mean(x), sd(x)),
                    mean_se    = c(mean(x), sd(x) / sqrt(length(x))),
                    median_IQR = c(median(x), quantile(x, c(.25, .75), type = 2)))
      expect_rounded(cont_cell(tbl, v), ref, 2)
    }
  }
})

test_that("geometric mean per visit is correct", {
  tbl <- sbv(vd, vars = "crp", stat_cont = "geomMean_sd", digits_cont = 2)
  for (v in VISITS) {
    x <- vd$crp[vd$visit == v & !is.na(vd$crp)]
    expect_true(abs(nums(cont_cell(tbl, v))[1] - exp(mean(log(x)))) <= 0.005 + 1e-8, label = v)
  }
})

test_that("continuous statistics by group (2 groups) are correct", {
  tbl <- sbv(vd, vars = "weight", group = "arm", digits_cont = 1)
  for (k in 1:2) for (v in VISITS) {
    x <- vd$weight[vd$visit == v & vd$arm == levels(vd$arm)[k] & !is.na(vd$weight)]
    expect_rounded(cont_cell(tbl, v, col = paste0("stat_", k)), c(median(x), min(x), max(x)), 1)
  }
})

test_that("continuous statistics by group (3 groups) are correct", {
  tbl <- sbv(vd, vars = "weight", group = "arm3", digits_cont = 1)
  for (k in 1:3) for (v in VISITS) {
    x <- vd$weight[vd$visit == v & vd$arm3 == levels(vd$arm3)[k] & !is.na(vd$weight)]
    expect_rounded(cont_cell(tbl, v, col = paste0("stat_", k)), c(median(x), min(x), max(x)), 1)
  }
})

test_that("overall column equals the ungrouped table", {
  tbl_g <- sbv(vd, vars = "weight", group = "arm", overall = TRUE)
  tbl_u <- sbv(vd, vars = "weight")
  for (v in VISITS) expect_equal(cont_cell(tbl_g, v, "stat_0"), cont_cell(tbl_u, v, "stat_0"))
})

test_that("digits_cont is respected", {
  for (dg in 0:2) {
    tbl  <- sbv(vd, vars = "weight", stat_cont = "mean_sd", digits_cont = dg)
    cell <- cont_cell(tbl, "Visit 1")
    dec  <- nchar(sub("^[^.]*\\.?", "", regmatches(cell, gregexpr("[0-9.]+", cell))[[1]]))
    expect_true(all(dec == dg), label = paste("digits_cont =", dg, ":", cell))
  }
})

test_that("visit with no data in one group shows an empty cell, not 'NA (Inf, -Inf)'", {
  d <- vd
  d$weight[d$visit == "Visit 3" & d$arm == "B"] <- NA
  tbl  <- sbv(d, vars = "weight", group = "arm")
  cell <- cont_cell(tbl, "Visit 3", col = "stat_2")
  expect_false(grepl("Inf", cell %||% ""))
})


# ---------------------------------------------------------------------------- #
# Categorical statistics ----
# ---------------------------------------------------------------------------- #

test_that("n (%) per visit is correct, missing in denominator (missing_percent = TRUE)", {
  tbl <- sbv(vd, vars = "ecog", stat_cat = "n_percent", digits_cat = 1, missing_percent = TRUE)
  for (v in VISITS) for (lv in levels(vd$ecog)) {
    x   <- vd$ecog[vd$visit == v]
    n   <- sum(x == lv, na.rm = TRUE)
    expect_rounded(cat_cell(tbl, v, lv), c(n, 100 * n / length(x)), 1)
  }
})

test_that("n (%) per visit is correct, missing NOT in denominator (missing_percent = FALSE)", {
  tbl <- sbv(vd, vars = "ecog", stat_cat = "n_percent", digits_cat = 1, missing_percent = FALSE)
  for (v in VISITS) for (lv in levels(vd$ecog)) {
    x <- vd$ecog[vd$visit == v & !is.na(vd$ecog)]
    n <- sum(x == lv)
    expect_rounded(cat_cell(tbl, v, lv), c(n, 100 * n / length(x)), 1)
  }
})

test_that("missing row shows the number of missing values per visit", {
  tbl <- sbv(vd, vars = "ecog", missing = TRUE, missing_text = "Missing")
  for (v in VISITS) {
    n_na <- sum(is.na(vd$ecog[vd$visit == v]))
    if (n_na > 0) expect_equal(nums(cat_cell(tbl, v, "Missing"))[1], n_na, label = v)
  }
})

test_that("missing_text is used", {
  tbl <- sbv(vd, vars = "ecog", missing_text = "Not done")
  expect_true("Not done" %in% tbl$table_body$label)
})

test_that("no missing row when missing = FALSE", {
  tbl <- sbv(vd, vars = "ecog", missing = FALSE)
  expect_false("Missing" %in% tbl$table_body$label)
})

test_that("no missing row for a visit without missing values", {
  d <- vd
  d$ecog[d$visit == "Visit 1" & is.na(d$ecog)] <- "0"
  tbl <- sbv(d, vars = "ecog")
  vi  <- visit_row(tbl, "Visit 1")
  v2  <- visit_row(tbl, "Visit 2")
  expect_false("Missing" %in% tbl$table_body$label[(vi + 1):(v2 - 1)])
})

test_that("categorical statistics by group are correct", {
  tbl <- sbv(vd, vars = "ecog", group = "arm", missing_percent = FALSE, digits_cat = 1)
  for (k in 1:2) for (v in VISITS) for (lv in levels(vd$ecog)) {
    x <- vd$ecog[vd$visit == v & vd$arm == levels(vd$arm)[k] & !is.na(vd$ecog)]
    n <- sum(x == lv)
    expect_rounded(cat_cell(tbl, v, lv, col = paste0("stat_", k)), c(n, 100 * n / length(x)), 1)
  }
})

test_that("stat_cat = 'n' and 'n_N' are correct", {
  tbl_n  <- sbv(vd, vars = "ecog", stat_cat = "n",   missing = FALSE, digits_cat = 0)
  tbl_nN <- sbv(vd, vars = "ecog", stat_cat = "n_N", missing = FALSE, digits_cat = 0)
  for (v in VISITS) for (lv in levels(vd$ecog)) {
    x <- vd$ecog[vd$visit == v & !is.na(vd$ecog)]
    expect_equal(nums(cat_cell(tbl_n, v, lv))[1], sum(x == lv))
    expect_equal(nums(cat_cell(tbl_nN, v, lv))[1:2], c(sum(x == lv), length(x)))
  }
})

test_that("continuous_as_categorical treats a numeric variable as categorical", {
  tbl <- sbv(vd, vars = "resp", continuous_as_categorical = "resp", missing = FALSE, digits_cat = 0)
  for (v in VISITS) {
    x <- vd$resp[vd$visit == v]
    expect_equal(nums(cat_cell(tbl, v, "1"))[1], sum(x == 1), label = v)
  }
})


# ---------------------------------------------------------------------------- #
# N columns and headers ----
# ---------------------------------------------------------------------------- #

test_that("add_n: N per visit = number of non-missing values (continuous, no group)", {
  tbl <- sbv(vd, vars = "weight", add_n = TRUE)
  expect_false(is_hidden(tbl, "n"))
  for (v in VISITS) {
    expect_equal(as.numeric(cont_cell(tbl, v, "n")),
                 sum(!is.na(vd$weight[vd$visit == v])), label = v)
  }
})

test_that("add_n: N per group and visit is correct (continuous)", {
  tbl <- sbv(vd, vars = "weight", group = "arm", add_n = TRUE)
  for (k in 1:2) for (v in VISITS) {
    ref <- sum(!is.na(vd$weight[vd$visit == v & vd$arm == levels(vd$arm)[k]]))
    expect_equal(as.numeric(cont_cell(tbl, v, paste0("add_n_stat_", k))), ref,
                 label = paste(v, levels(vd$arm)[k]))
  }
})

test_that("add_n: N per visit is correct (categorical)", {
  tbl <- sbv(vd, vars = "ecog", add_n = TRUE)
  tb  <- tbl$table_body
  for (v in VISITS) {
    ref <- sum(!is.na(vd$ecog[vd$visit == v]))
    vi  <- visit_row(tbl, v)
    # N is shown on the visit row or the first row below it
    expect_true(ref %in% suppressWarnings(as.numeric(tb$n[vi:(vi + 1)])), label = v)
  }
})

test_that("N columns are hidden when add_n = FALSE", {
  tbl <- sbv(vd, vars = "weight", group = "arm", add_n = FALSE)
  for (col in c("n", "add_n_stat_1", "add_n_stat_2")) expect_true(is_hidden(tbl, col), label = col)
})

test_that("overall column hidden when overall = FALSE, shown when TRUE", {
  expect_true(is_hidden(sbv(vd, vars = "weight", group = "arm", overall = FALSE), "stat_0"))
  expect_false(is_hidden(sbv(vd, vars = "weight", group = "arm", overall = TRUE), "stat_0"))
})

test_that("header N = largest number of observations in any visit (per group)", {
  tbl <- sbv(vd, vars = "weight", group = "arm", overall = TRUE)
  tab <- table(vd$visit, vd$arm)
  for (k in 1:2) {
    expect_match(header_label(tbl, paste0("stat_", k)), paste0("N = ", max(tab[, k])), fixed = TRUE)
  }
  expect_match(header_label(tbl, "stat_0"), paste0("N = ", max(table(vd$visit))), fixed = TRUE)
})

test_that("headers are the same for continuous and categorical variables (stacking)", {
  tbl_c <- sbv(vd, vars = "weight", group = "arm")
  tbl_k <- sbv(vd, vars = "ecog",   group = "arm")
  expect_equal(header_label(tbl_c, "stat_1"), header_label(tbl_k, "stat_1"))
  expect_equal(header_label(tbl_c, "stat_2"), header_label(tbl_k, "stat_2"))
})


# ---------------------------------------------------------------------------- #
# visitgroup ----
# ---------------------------------------------------------------------------- #

test_that("visitgroup rows are shown and statistics stay correct", {
  tbl <- sbv(vd, vars = "weight", visitgroup = "period", order = "visit_order")
  expect_true(all(c("Baseline", "Follow-up") %in% tbl$table_body$label))
  for (v in VISITS) {
    x <- vd$weight[vd$visit == v & !is.na(vd$weight)]
    expect_rounded(cont_cell(tbl, v), c(median(x), min(x), max(x)), 1)
  }
})

test_that("visitgroup works with a group and a categorical variable", {
  expect_no_error(sbv(vd, vars = c("weight", "ecog"), group = "arm",
                      visitgroup = "period", order = "visit_order", add_n = TRUE))
})


# ---------------------------------------------------------------------------- #
# Several variables / labels ----
# ---------------------------------------------------------------------------- #

test_that("variables are stacked in the order given, statistics stay correct", {
  tbl <- sbv(vd, vars = c("ecog", "weight"))
  lab <- tbl$table_body$label
  expect_true(which(lab == "ECOG") < which(lab == "Weight (kg)"))
  # weight statistics in the second block
  for (v in VISITS) {
    x <- vd$weight[vd$visit == v & !is.na(vd$weight)]
    expect_rounded(cont_cell(tbl, v, after = "Weight (kg)"), c(median(x), min(x), max(x)), 1)
  }
})

test_that("labels: from attribute, from `labels`, else variable name", {
  tbl <- sbv(vd, vars = c("weight", "ecog", "crp"), labels = list(ecog = "ECOG performance status"))
  lab <- tbl$table_body$label
  expect_true("Weight (kg)" %in% lab)
  expect_true("ECOG performance status" %in% lab)
  expect_true("crp" %in% lab)
})


# ---------------------------------------------------------------------------- #
# Word output / input checks ----
# ---------------------------------------------------------------------------- #

test_that("Word file is written", {
  f <- tempfile(fileext = ".docx")
  expect_message(
    summaryByVisit(vd, vars = "weight", word_output = TRUE, file_path = f),
    "Table saved to"
  )
  expect_true(file.exists(f))
})

test_that("invalid input gives an error", {
  expect_error(sbv(vd, vars = "foo"))
  expect_error(sbv(vd, vars = "weight", group = "foo"))
  expect_error(sbv(vd, vars = "weight", visit = "foo"))
  expect_error(sbv(vd, vars = "weight", stat_cont = "mean"))
  expect_error(sbv(vd, vars = "ecog", stat_cat = "percent"))
})

test_that("more than 3 groups gives a clear error", {
  d <- vd
  d$arm4 <- factor(rep(c("A", "B", "C", "D"), length.out = nrow(d)))
  expect_error(sbv(d, vars = "weight", group = "arm4"), "3")
})






