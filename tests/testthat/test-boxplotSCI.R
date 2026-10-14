# Tests for boxplotSCI()
#
# The focus is on the numbers the plot is built from: box statistics, patient
# counts under the x-axis, box positions/widths, axis limits and ticks, colours
# and legend entries. Expected values are worked out by hand (see comments and
# make_fixture() in helper-boxplotSCI.R) rather than re-computed with the same
# formulas as the function, so a change in behaviour shows up as a failure.

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("missing data or unknown var column raise informative errors", {
  expect_error(boxplotSCI(), "'data' must be specified")
  expect_error(
    capture_boxplotSCI(data = make_fixture(), var = "does_not_exist"),
    "'var' column not found"
  )
})

test_that("bare and quoted column names give identical numbers", {
  df <- make_fixture()
  bare   <- capture_boxplotSCI(data = df, var = value, order = time, group = arm)
  quoted <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm")

  expect_identical(box_stats(bare), box_stats(quoted))
  expect_identical(box_n(bare), box_n(quoted))
  expect_identical(n_rows(bare), n_rows(quoted))
})


# ---------------------------------------------------------------------------
# Box statistics (longitudinal, several groups)
# ---------------------------------------------------------------------------

test_that("box statistics per group and timepoint match hand-computed values", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value",
                            order = "time", group = "arm")

  expect_length(rec$boxplot, 2) # one boxplot() call per arm

  # Arm A: rows = lower whisker, lower hinge, median, upper hinge, upper whisker
  expect_equal(
    box_stats(rec)[[1]],
    cbind(c(1, 2, 3, 4, 5), c(10, 20, 30, 40, 50), c(2, 4, 6, 8, 8)),
    ignore_attr = TRUE
  )
  expect_equal(box_n(rec)[[1]], c(5, 5, 5))
  expect_equal(rec$boxplot[[1]]$value$out, 100)   # outlier at t = 3
  expect_equal(rec$boxplot[[1]]$value$group, 3)   # ... belongs to timepoint 3

  # Arm B: NA value ignored at t = 1, even n at t = 2, empty at t = 3
  stats_b <- box_stats(rec)[[2]]
  expect_equal(stats_b[, 1], c(5, 6, 7, 8, 9))
  expect_equal(stats_b[, 2], c(1, 2, 4, 6, 7))
  expect_true(all(is.na(stats_b[, 3])))
  expect_equal(box_n(rec)[[2]], c(5, 4, 0))
  expect_length(rec$boxplot[[2]]$value$out, 0)
})

test_that("box statistics agree with boxplot.stats() on random data", {
  set.seed(20261008)
  df <- expand.grid(id = 1:40, time = c(0, 3, 6, 12), arm = c("Ctl", "Trt"))
  df$value <- round(rnorm(nrow(df), mean = 100 + df$time, sd = 15), 1)
  df$value[sample(nrow(df), 25)] <- NA            # sprinkle missing values
  df <- df[sample(nrow(df), 260), ]               # unbalanced and unsorted

  rec <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm")

  arms  <- levels(factor(df$arm))
  times <- sort(unique(df$time))
  for (g in seq_along(arms)) {
    for (t in seq_along(times)) {
      x <- df$value[df$arm == arms[g] & df$time == times[t]]
      x <- x[!is.na(x)]
      expected <- grDevices::boxplot.stats(x)
      expect_equal(box_stats(rec)[[g]][, t], expected$stats,
                   info = paste(arms[g], times[t]))
      expect_equal(box_n(rec)[[g]][t], expected$n,
                   info = paste(arms[g], times[t]))
    }
  }
})

test_that("rows with missing group or missing time do not leak into any box", {
  df <- make_fixture()
  extra <- data.frame(
    value = c(1000, 1000, -1000),
    time  = c(1, NA, 2),
    visit = c("Baseline", NA, "Week 4"),
    arm   = c(NA, "A", NA),
    stringsAsFactors = FALSE
  )
  clean <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm")
  dirty <- capture_boxplotSCI(data = rbind(df, extra), var = "value",
                              order = "time", group = "arm")

  expect_equal(box_stats(dirty), box_stats(clean))
  expect_equal(n_rows(dirty), n_rows(clean))
})

# ---------------------------------------------------------------------------
# Patient counts below the x-axis
# ---------------------------------------------------------------------------

test_that("patient counts exclude missing values and sit on the right lines", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value",
                            order = "time", group = "arm")

  expect_equal(n_rows(rec), list(c(5, 5, 5), c(5, 4, 0)), ignore_attr = TRUE)
  expect_equal(n_row_lines(rec), c(2, 2.65))
  for (x in Filter(function(x) is.null(x$args$adj), rec$mtext)) {
    expect_equal(x$args$at, 1:3)
  }
})

test_that("patient counts equal the n used for each box", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value",
                            order = "time", group = "arm")
  expect_equal(lapply(n_rows(rec), as.numeric), box_n(rec))
})

test_that("missing_nr replaces zero counts only", {
  df <- make_fixture()

  rec_na <- capture_boxplotSCI(data = df, var = "value", order = "time",
                               group = "arm", missing_nr = NA)
  expect_equal(n_rows(rec_na)[[1]], c(5, 5, 5), ignore_attr = TRUE)
  expect_equal(n_rows(rec_na)[[2]], c(5, 4, NA), ignore_attr = TRUE)

  rec_dash <- capture_boxplotSCI(data = df, var = "value", order = "time",
                                 group = "arm", missing_nr = "-")
  expect_equal(n_rows(rec_dash)[[2]], c("5", "4", "-"), ignore_attr = TRUE)
})

test_that("unused factor levels of the group are dropped (as documented)", {
  # group_names must match levels(factor(data$group)), and factor() drops
  # empty levels, so an arm without data simply disappears from the plot.
  df <- make_fixture()
  df$arm <- factor(df$arm, levels = c("Z", "A", "B"))

  rec <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm")

  expect_length(rec$boxplot, 2)
  expect_equal(n_rows(rec), list(c(5, 5, 5), c(5, 4, 0)), ignore_attr = TRUE)
  expect_equal(group_headers(rec), c("# A", "# B"))
})

test_that("group headers use group_names_short, falling back to group_names", {
  df <- make_fixture()

  rec <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm",
                            group_names = c("Treatment", "Control"),
                            group_names_short = c("Trt", "Ctl"))
  expect_equal(group_headers(rec), c("# Trt", "# Ctl"))

  rec2 <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm",
                             group_names = c("Treatment", "Control"))
  expect_equal(group_headers(rec2), c("# Treatment", "# Control"))
})

# ---------------------------------------------------------------------------
# Box positions and widths
# ---------------------------------------------------------------------------

test_that("2 groups x 3 timepoints: box width, shift and positions", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value",
                            order = "time", group = "arm")

  # cluster width = 0.85 - 0.15 * 3/12 = 0.8125
  # boxwex        = 0.8125 / (1.15 * 1 + 1) = 0.8125 / 2.15
  # shift         = 1.15 * boxwex; groups at -shift/2 and +shift/2
  boxwex <- 0.8125 / 2.15
  shift  <- 1.15 * boxwex
  expect_equal(rec$boxplot[[1]]$args$boxwex, boxwex)
  expect_equal(box_at(rec)[[1]], 1:3 - shift / 2)
  expect_equal(box_at(rec)[[2]], 1:3 + shift / 2)

  # boxes of neighbouring groups must not overlap
  expect_gt(box_at(rec)[[2]][1] - box_at(rec)[[1]][1], boxwex)
})

test_that("3 groups x 2 timepoints: offsets are symmetric around the timepoint", {
  df <- data.frame(value = 1:12,
                   time  = rep(c(1, 2), 6),
                   arm   = rep(c("A", "B", "C"), each = 4))
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm")

  # cluster width = 0.85 - 0.15 * 2/12 = 0.825; boxwex = 0.825 / 3.3 = 0.25
  expect_equal(rec$boxplot[[1]]$args$boxwex, 0.25)
  expect_equal(box_at(rec)[[1]], c(1, 2) - 0.2875)
  expect_equal(box_at(rec)[[2]], c(1, 2))
  expect_equal(box_at(rec)[[3]], c(1, 2) + 0.2875)
  expect_equal(n_row_lines(rec), c(2, 2.65, 3.3))
})

test_that("single group: boxes are centred on the timepoints", {
  df <- subset(make_fixture(), arm == "A")
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time")

  expect_length(rec$boxplot, 1)
  expect_equal(box_at(rec)[[1]], 1:3)
  expect_equal(rec$boxplot[[1]]$args$boxwex, 0.65 - 0.15 * 3 / 12)
  expect_equal(n_rows(rec), list(c(5, 5, 5)), ignore_attr = TRUE)
  expect_length(group_headers(rec), 0)
})

test_that("box width shrinks with more timepoints and is capped at 12", {
  mk <- function(k) data.frame(value = seq_len(3 * k), time = rep(seq_len(k), 3))
  wex <- function(k) capture_boxplotSCI(data = mk(k), var = "value",
                                        order = "time")$boxplot[[1]]$args$boxwex
  expect_equal(wex(10), 0.65 - 0.15 * 10 / 12)
  expect_equal(wex(12), 0.5)
  expect_equal(wex(20), 0.5)
})

test_that("user supplied boxwex and shift are used as given", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value", order = "time",
                            group = "arm", boxwex = 0.2, shift = 0.3)
  expect_equal(rec$boxplot[[1]]$args$boxwex, 0.2)
  expect_equal(box_at(rec)[[1]], 1:3 - 0.15)
  expect_equal(box_at(rec)[[2]], 1:3 + 0.15)
})

# ---------------------------------------------------------------------------
# Timepoint ordering and x-axis labels
# ---------------------------------------------------------------------------

test_that("numeric timepoints are sorted and equally spaced, data order irrelevant", {
  df <- data.frame(value = c(30, 31, 32, 10, 11, 12, 20, 21, 22),
                   time  = c(12, 12, 12, 0, 0, 0, 4, 4, 4))
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time")

  ax <- axis_call(rec, 1)
  expect_equal(ax$at, 1:3)
  expect_equal(ax$labels, c(0, 4, 12))
  expect_equal(box_stats(rec)[[1]][3, ], c(11, 21, 31))   # medians in time order
})

test_that("factor timepoints follow level order and unused levels are dropped", {
  lv <- c("Screening", "C1", "C2", "EOT", "Follow-up")
  df <- data.frame(value = c(40, 41, 10, 11, 30, 31, 20, 21),
                   time  = factor(c("EOT", "EOT", "Screening", "Screening",
                                    "C2", "C2", "C1", "C1"), levels = lv))
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time")

  ax <- axis_call(rec, 1)
  expect_equal(as.character(ax$labels), c("Screening", "C1", "C2", "EOT"))
  expect_equal(ax$at, 1:4)
  expect_equal(box_stats(rec)[[1]][3, ], c(10.5, 20.5, 30.5, 40.5))
})

test_that("visit labels are aligned with the sorted timepoints", {
  df <- make_fixture()
  df <- df[c(25:1), ]   # reverse the rows
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time",
                            visit = "visit", group = "arm")
  expect_equal(axis_call(rec, 1)$labels, c("Baseline", "Week 4", "Week 8"))
})

# ---------------------------------------------------------------------------
# Y-axis limits and ticks
# ---------------------------------------------------------------------------

test_that("default ylim pads the data range by 10% and is passed to every box", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value",
                            order = "time", group = "arm")
  # range 1..100 -> c(1 - 0.1, 100 + 10)
  for (b in rec$boxplot) expect_equal(b$args$ylim, c(0.9, 110))
  # ticks: seq(floor(0.9), ceiling(110), length.out = 5)
  expect_equal(axis_call(rec, 2)$at, c(0, 27.5, 55, 82.5, 110))
  expect_equal(axis_call(rec, 2)$labels, c(0, 27.5, 55, 82.5, 110))
})

test_that("default ylim is correct for positive, mixed and negative data", {
  ylim_for <- function(v) {
    rec <- capture_boxplotSCI(data = data.frame(value = v, time = rep(1:2, length.out = length(v))),
                              var = "value", order = "time")
    rec$boxplot[[1]]$args$ylim
  }
  expect_equal(ylim_for(c(10, 20, 30, 50)),   c(9, 55))
  expect_equal(ylim_for(c(-20, 0, 10, 40)),   c(-22, 44))
  expect_equal(ylim_for(c(-50, -30, -20, -10)), c(-55, -9))
})

test_that("default ylim always contains all data", {
  set.seed(1)
  for (i in 1:20) {
    v <- rnorm(30, mean = runif(1, -100, 100), sd = runif(1, 1, 50))
    rec <- capture_boxplotSCI(data = data.frame(value = v, time = rep(1:3, 10)),
                              var = "value", order = "time")
    yl <- rec$boxplot[[1]]$args$ylim
    expect_true(yl[1] <= min(v) && yl[2] >= max(v))
  }
})

test_that("user ylim and y_ticks override the defaults", {
  df <- make_fixture()
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time",
                            group = "arm", ylim = c(0, 200))
  expect_equal(rec$boxplot[[2]]$args$ylim, c(0, 200))
  expect_equal(axis_call(rec, 2)$at, c(0, 50, 100, 150, 200))

  rec2 <- capture_boxplotSCI(data = df, var = "value", order = "time",
                             group = "arm", y_ticks = c(0, 25, 100))
  expect_equal(axis_call(rec2, 2)$at, c(0, 25, 100))
})

# ---------------------------------------------------------------------------
# Single timepoint mode (no `order`)
# ---------------------------------------------------------------------------

test_that("without order, one box per group with correct statistics", {
  df <- data.frame(value = c(1, 2, 3, 4, 5,  10, 20, 30, 40, 50,  2, 4, 6, 8, 100),
                   arm   = rep(c("A", "B", "C"), each = 5))
  rec <- capture_boxplotSCI(data = df, var = "value", group = "arm",
                            group_names = c("Arm A", "Arm B", "Arm C"))

  expect_length(rec$boxplot, 1)
  expect_equal(box_stats(rec)[[1]],
               cbind(c(1, 2, 3, 4, 5), c(10, 20, 30, 40, 50), c(2, 4, 6, 8, 8)),
               ignore_attr = TRUE)
  expect_equal(box_n(rec)[[1]], c(5, 5, 5))
  expect_equal(rec$boxplot[[1]]$value$out, 100)

  expect_equal(rec$boxplot[[1]]$args$boxwex, 0.65 - 0.15 * 3 / 12)
  expect_equal(axis_call(rec, 1)$at, 1:3)
  expect_equal(axis_call(rec, 1)$labels, c("Arm A", "Arm B", "Arm C"))
  expect_equal(title_arg(rec, "xlab")$xlab, "Group")
  expect_equal(title_arg(rec, "xlab")$line, 3)
  expect_equal(margins(rec), c(4.1, 4.6, 4.1, 2.1))
  expect_length(rec$mtext, 0) # no patient counts in this mode
})

test_that("a single observed timepoint is treated like no order", {
  df <- data.frame(value = 1:10, time = 1, arm = rep(c("A", "B"), each = 5))
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm")
  expect_length(rec$boxplot, 1)
  expect_equal(box_stats(rec)[[1]][3, ], c(3, 8))
  expect_equal(title_arg(rec, "xlab")$xlab, "Group")
})

# ---------------------------------------------------------------------------
# Layout numbers: margins and label offsets
# ---------------------------------------------------------------------------

test_that("dynamic margins and xlab line follow the number of groups/timepoints", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value",
                            order = "time", group = "arm")
  # bottom = 5.0 + (2 - 1) * 0.65; left = 4.5; all + 0.1
  expect_equal(margins(rec), c(5.75, 4.6, 4.1, 2.1))
  expect_equal(title_arg(rec, "xlab")$line, 2 + 0.65 + 1.2)
  expect_equal(title_arg(rec, "xlab")$xlab, "Time")
  expect_equal(title_arg(rec, "ylab")$line, 3)

  df10 <- data.frame(value = 1:30, time = rep(1:10, 3))
  rec10 <- capture_boxplotSCI(data = df10, var = "value", order = "time")
  expect_equal(margins(rec10), c(6.6, 5.6, 4.1, 2.1))
})

test_that("mar_custom, xlab_line and ylab_line are used as given", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value", order = "time",
                            group = "arm", mar_custom = c(8, 6, 2, 1),
                            xlab_line = 5.5, ylab_line = 2)
  expect_equal(margins(rec), c(8, 6, 2, 1))
  expect_equal(title_arg(rec, "xlab")$line, 5.5)
  expect_equal(title_arg(rec, "ylab")$line, 2)
})

# ---------------------------------------------------------------------------
# Colours and legend
# ---------------------------------------------------------------------------

test_that("default SCI palette is applied per group (1-5 groups)", {
  palettes <- list(
    "#E4002B",
    c("#F4B9C2", "#E4002B"),
    c("#FFE3E3", "#F4B9C2", "#E4002B"),
    c("#FFE3E3", "#F4B9C2", "#E4002B", "#961D13"),
    c("#FFE3E3", "#F4B9C2", "#E4002B", "#961D13", "#401F28")
  )
  for (k in 1:5) {
    df <- data.frame(value = seq_len(4 * k),
                     time  = rep(1:2, 2 * k),
                     arm   = rep(LETTERS[1:k], each = 4))
    rec <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm")
    box_cols <- vapply(rec$boxplot, function(b) b$args$col, character(1))
    expect_equal(box_cols, palettes[[k]], info = paste(k, "groups"))
  }
})

test_that("more than 5 groups get an interpolated palette of the right length", {
  df <- data.frame(value = 1:28, time = rep(1:2, 14), arm = rep(LETTERS[1:7], each = 4))
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm")
  box_cols <- vapply(rec$boxplot, function(b) b$args$col, character(1))
  expect_length(unique(box_cols), 7)
  expect_equal(box_cols[1], "#FFE3E3")
  expect_equal(box_cols[7], "#401F28")
})

test_that("user colours map to groups and legend in level order", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value", order = "time",
                            group = "arm", col = c("lightblue", "orange"),
                            group_names = c("Treatment", "Control"))
  expect_equal(rec$boxplot[[1]]$args$col, "lightblue")
  expect_equal(rec$boxplot[[2]]$args$col, "orange")

  lg <- rec$legend[[1]]$args
  expect_equal(lg$legend, c("Treatment", "Control"))
  expect_equal(lg$fill, c("lightblue", "orange"))
})

test_that("single group legend falls back to the variable name", {
  df <- subset(make_fixture(), arm == "A")
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time")
  lg <- rec$legend[[1]]$args
  expect_equal(lg$legend, "value")
  expect_equal(lg$fill, "#E4002B")
})

test_that("color_time colours each timepoint and labels the legend with them", {
  df <- subset(make_fixture(), arm == "A")
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time",
                            visit = "visit", color_time = TRUE)
  pal <- c("#FFE3E3", "#F4B9C2", "#E4002B")
  expect_equal(rec$boxplot[[1]]$args$col, pal)
  expect_equal(rec$legend[[1]]$args$legend, c("Baseline", "Week 4", "Week 8"))
  expect_equal(rec$legend[[1]]$args$fill, pal)
})

test_that("numeric legend position is shifted by legend_offset", {
  rec <- capture_boxplotSCI(data = make_fixture(), var = "value", order = "time",
                            group = "arm", legend_position = c(2, 50),
                            legend_offset = c(0.5, -5))
  lg <- rec$legend[[1]]$args
  expect_equal(lg$x, 2.5)
  expect_equal(lg$y, 45)
})

test_that("no legend for a single group at a single timepoint", {
  df <- data.frame(value = 1:5, arm = "A")
  rec <- capture_boxplotSCI(data = df, var = "value", group = "arm")
  expect_length(rec$legend, 0)
})

# ---------------------------------------------------------------------------
# Reference lines
# ---------------------------------------------------------------------------

test_that("reference lines are drawn at the requested heights and styles", {
  df <- make_fixture()
  rec <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm",
                            lines = c(20, 60), lty = c(2, 3),
                            col_lines = c("red", "blue"))
  ab <- rec$abline[[1]]$args
  expect_equal(ab$h, c(20, 60))
  expect_equal(ab$lty, c(2, 3))
  expect_equal(ab$col, c("red", "blue"))

  rec2 <- capture_boxplotSCI(data = df, var = "value", order = "time",
                             group = "arm", lines = 50)
  expect_equal(rec2$abline[[1]]$args$lty, 1)
  expect_equal(rec2$abline[[1]]$args$col, "grey")

  rec3 <- capture_boxplotSCI(data = df, var = "value", order = "time", group = "arm")
  expect_length(rec3$abline, 0)
})
