# Run plotting code on a null device, so that tests draw nothing
draw_quietly <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(expr)
}

# The arguments of every low-level graphics call 'fun' (e.g. "C_rect",
# "C_mtext", "C_segments") that 'expr' draws, read from the display list
drawn_calls <- function(expr, fun) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  grDevices::dev.control("enable")
  force(expr)
  dl <- grDevices::recordPlot()[[1]]
  lapply(Filter(function(e) identical(e[[2]][[1]]$name, fun), dl),
         function(e) e[[2]][-1])
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
  # nothing left to compare when all are missing (e.g. no interval drawn)
  if (any(ok))
    testthat::expect_lte(max(abs(parsed[ok] - truth[ok])),
                         k * 0.5 * 10^(-digits) + 1e-9)
}

# The x-axis ticks and labels a plot draws (the axis() calls with ticks;
# plot() and bxp() record empty ones)
x_axis_drawn <- function(p) {
  ax <- drawn_calls(print(p), "C_axis")
  ax <- Filter(function(a) a[[1]] == 1 && !is.null(a[[2]]) && !is.null(a[[3]]), ax)
  list(at = ax[[length(ax)]][[2]], labels = ax[[length(ax)]][[3]])
}

# The boxes a plot draws: one element per draw_boxes() call (per group over
# time), each with the box statistics (rows: lower whisker, Q1, median, Q3,
# upper whisker; one column per box), n and the outliers
boxes_drawn <- function(p) {
  rec  <- new.env(parent = emptyenv())
  rec$z <- list()
  real <- draw_boxes
  testthat::local_mocked_bindings(
    draw_boxes = function(...) {
      z <- real(...)
      rec$z <- c(rec$z, list(z))
      invisible(z)
    },
    .package = "summarySCI")
  draw_quietly(print(p))
  rec$z
}

