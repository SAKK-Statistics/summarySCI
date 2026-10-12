# Helpers for test-boxplotSCI.R
#
# boxplotSCI() draws a plot and returns nothing useful, so to check the numbers
# we spy on the graphics functions it imports (boxplot, mtext, axis, legend,
# par, abline, title). Every call is still forwarded to the real function on a
# null PDF device, and we record both the arguments and the return value
# (for boxplot() that is the computed stats: whiskers, hinges, median, n, out).

capture_boxplotSCI <- function(...) {
  rec <- new.env(parent = emptyenv())
  for (nm in c("boxplot", "mtext", "axis", "legend", "par", "abline", "title")) {
    rec[[nm]] <- list()
  }

  spy <- function(name, real) {
    force(name)
    force(real)
    function(...) {
      out <- real(...)
      rec[[name]] <- c(rec[[name]], list(list(args = list(...), value = out)))
      invisible(out)
    }
  }

  grDevices::pdf(NULL)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)

  testthat::local_mocked_bindings(
    boxplot = spy("boxplot", graphics::boxplot),
    mtext   = spy("mtext",   graphics::mtext),
    axis    = spy("axis",    graphics::axis),
    legend  = spy("legend",  graphics::legend),
    par     = spy("par",     graphics::par),
    abline  = spy("abline",  graphics::abline),
    title   = spy("title",   graphics::title),
    .package = "summarySCI"
  )

  boxplotSCI(...)
  as.list(rec)
}

# ---- Accessors for the recorded calls --------------------------------------

# One element per boxplot() call (= per group in longitudinal mode)
box_stats <- function(rec) lapply(rec$boxplot, function(x) x$value$stats)
box_n     <- function(rec) lapply(rec$boxplot, function(x) x$value$n)
box_at    <- function(rec) lapply(rec$boxplot, function(x) x$args$at)

# Patient-count rows below the x-axis (the "# group" headers carry adj = 1)
n_rows <- function(rec) {
  calls <- Filter(function(x) is.null(x$args$adj), rec$mtext)
  lapply(calls, function(x) x$args$text)
}
n_row_lines <- function(rec) {
  calls <- Filter(function(x) is.null(x$args$adj), rec$mtext)
  vapply(calls, function(x) x$args$line, numeric(1))
}
group_headers <- function(rec) {
  calls <- Filter(function(x) !is.null(x$args$adj), rec$mtext)
  vapply(calls, function(x) x$args$text, character(1))
}

axis_call <- function(rec, side) {
  for (x in rec$axis) {
    s <- if (!is.null(x$args$side)) x$args$side else x$args[[1]]
    if (s == side) return(x$args)
  }
  NULL
}

margins <- function(rec) {
  for (x in rec$par) if ("mar" %in% names(x$args)) return(x$args$mar)
  NULL
}

title_arg <- function(rec, what) {
  for (x in rec$title) if (what %in% names(x$args)) return(x$args)
  NULL
}

# ---- Fixture with hand-checkable numbers -----------------------------------
#
#   Arm A, t = 1 : 1, 2, 3, 4, 5           -> stats 1, 2, 3, 4, 5      n = 5
#   Arm A, t = 2 : 10, 20, 30, 40, 50      -> stats 10, 20, 30, 40, 50 n = 5
#   Arm A, t = 3 : 2, 4, 6, 8, 100         -> hinges 4 / 8, IQR 4,
#                                             upper fence 8 + 6 = 14,
#                                             so 100 is an outlier and the
#                                             upper whisker stops at 8
#                                          -> stats 2, 4, 6, 8, 8 n = 5, out 100
#   Arm B, t = 1 : 5, 6, 7, 8, 9 (+ one NA)-> stats 5, 6, 7, 8, 9      n = 5
#   Arm B, t = 2 : 1, 3, 5, 7              -> fivenum 1, 2, 4, 6, 7    n = 4
#   Arm B, t = 3 : no observations         -> all NA                   n = 0
make_fixture <- function() {
  data.frame(
    value = c(1, 2, 3, 4, 5,
              10, 20, 30, 40, 50,
              2, 4, 6, 8, 100,
              5, 6, 7, 8, 9, NA,
              1, 3, 5, 7),
    time  = c(rep(1, 5), rep(2, 5), rep(3, 5), rep(1, 6), rep(2, 4)),
    visit = c(rep("Baseline", 5), rep("Week 4", 5), rep("Week 8", 5),
              rep("Baseline", 6), rep("Week 4", 4)),
    arm   = c(rep("A", 15), rep("B", 10)),
    stringsAsFactors = FALSE
  )
}
