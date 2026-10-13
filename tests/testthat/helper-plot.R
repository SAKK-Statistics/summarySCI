# Run plotting code on a null device, so that tests draw nothing
draw_quietly <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(expr)
}

# Hemoglobin data with a categorical version of hb (missing where hb is
# missing) and a visit grouping
hb_fixture <- function() {
  env <- new.env()
  utils::data("hemoglobin_data", package = "summarySCI", envir = env)
  d <- env$hemoglobin_data
  d$hb_cat <- as.character(cut(d$hb, c(-Inf, 120, 140, Inf),
                               labels = c("Low", "Normal", "High")))
  d$vg <- factor(ifelse(d$visit_nr <= 2, "Baseline", "Treatment"),
                 levels = c("Baseline", "Treatment"))
  d
}

# Three arms, values with thousands separators and negative signs once printed
three_arm_fixture <- function() {
  set.seed(2026)
  n <- 240
  d <- data.frame(
    visit = paste("Visit", rep(1:4, each = n / 4)),
    arm   = rep(c("Arm A", "Arm B", "Arm C"), length.out = n),
    value = round(stats::rnorm(n, mean = 1500, sd = 2500), 2),
    grade = sample(c("G1", "G2", "G3", "G4"), n, replace = TRUE,
                   prob = c(0.4, 0.3, 0.2, 0.1)),
    stringsAsFactors = FALSE)
  d$value[c(3, 50, 121)] <- NA
  d$grade[c(7, 70, 200, 201)] <- NA
  d
}

# Non-missing values of 'var' for one group and visit of the raw data
cell <- function(d, var, group_col, group, visit) {
  x <- d[[var]][d$visit == visit &
                  (if (is.null(group_col)) TRUE else d[[group_col]] == group)]
  x[!is.na(x)]
}

# Every parsed number lies within the rounding of the printed table
expect_within_rounding <- function(parsed, truth, digits, k = 1) {
  ok <- !is.na(truth)
  testthat::expect_identical(is.na(parsed), is.na(truth))
  testthat::expect_lte(max(abs(parsed[ok] - truth[ok])),
                       k * 0.5 * 10^(-digits) + 1e-9)
}
