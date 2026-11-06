# Slow (several minutes): run locally and on GitHub, not on CRAN
skip_on_cran()

# One time point of the hemoglobin data, with a categorical and a Yes/No
# version of hb (both missing where hb is missing)
visit_fixture <- function(v = 2) {
  d <- hb_fixture()
  d <- d[d$visit_nr == v, ]
  d$anemia <- ifelse(d$hb < 120, "Yes", "No")
  d
}

# Counts and percentages of a categorical variable as printed in a
# summaryTable() table: one row per level and group
table_counts <- function(tbl, var) {
  tb  <- as.data.frame(tbl$table_body)
  hdr <- tbl$table_styling$header
  cols <- hdr$column[grepl("^stat_[0-9]+$", hdr$column) & !hdr$hide]
  grp  <- trimws(sub("\\n.*$", "", gsub("\\*\\*", "", hdr$label[match(cols, hdr$column)])))
  rows <- tb[tb$variable == var & tb$row_type %in% c("level", "missing"), ]
  do.call(rbind, lapply(seq_along(cols), function(k) {
    s <- rows[[cols[k]]]
    data.frame(level = rows$label, group = grp[k], count = parse_count(s),
               percent = as.numeric(sub("^.*\\(<?([0-9.]+)%\\).*$", "\\1",
                                        ifelse(grepl("%", s), s, NA))),
               stringsAsFactors = FALSE)
  }))
}

# Every count and percentage of the stacked bars equals the table's
check_bars_match_table <- function(res, var, groups, missing_text = "Missing") {
  tc  <- table_counts(res$table, var)
  num <- res$plots[[var]]$numbers
  for (i in seq_len(nrow(tc))) {
    lev <- tc$level[i]
    if (!lev %in% dimnames(num$counts)[[1]]) {
      # only the table's missing row may be absent from the plot, and then
      # only when it has no percentage there
      expect_equal(lev, missing_text)
      next
    }
    g <- groups[[tc$group[i]]]
    expect_equal(num$counts[lev, g, 1], tc$count[i],
                 label = paste(var, lev, g, "count"))
    if (!is.na(tc$percent[i]))
      expect_within_rounding(num$percent[lev, g, 1], tc$percent[i], digits = 0)
  }
}

test_that("type chooses the table, the plots or both", {
  d <- visit_fixture()
  res <- summarizeSCI(d, vars = "hb", group = "Arm", type = "table")
  expect_s3_class(res, "summarizeSCI")
  expect_s3_class(res$table, "flextable")
  expect_length(res$plots, 0)
  res <- summarizeSCI(d, vars = "hb", group = "Arm", type = "table",
                      as_flex_table = FALSE)
  expect_s3_class(res$table, "gtsummary")

  res <- summarizeSCI(d, vars = "hb", group = "Arm", type = "plot")
  expect_null(res$table)
  expect_named(res$plots, "hb")
  expect_output(print(res), "hb +boxplot")

  res <- summarizeSCI(d, vars = "hb", group = "Arm")      # default: both
  expect_s3_class(res$table, "flextable")
  expect_named(res$plots, "hb")
  expect_error(summarizeSCI(d, vars = "hb", type = "plots"), "should be one of")
})

test_that("each variable gets the plot of its type in the table", {
  d <- visit_fixture()
  res <- summarizeSCI(d, vars = c("hb", "hb_cat", "anemia"), group = "Arm",
                      as_flex_table = FALSE)
  expect_named(res$plots, c("hb", "hb_cat", "anemia"))
  expect_equal(vapply(res$plots, function(p) p$type, ""),
               c(hb = "boxplot", hb_cat = "stacked barplot",
                 anemia = "stacked barplot"))
  expect_s3_class(res$plots$hb, "sci_plot")
  expect_no_error(draw_quietly(print(res$plots$hb_cat)))
  expect_output(print(res), "hb_cat +stacked barplot")
})

test_that("the bars show the numbers of the table", {
  # visit 1: most hb values are missing, so a wrong denominator shows
  d <- visit_fixture(1)
  arms <- c("Arm A" = "Arm A", "Arm B" = "Arm B")
  for (mp in list(TRUE, FALSE)) {
    res <- summarizeSCI(d, vars = c("hb_cat", "anemia"), group = "Arm",
                        missing_percent = mp, as_flex_table = FALSE)
    # missing values form a segment exactly when the table's percentages count them
    expect_equal("Missing" %in% dimnames(res$plots$hb_cat$numbers$counts)[[1]], mp)
    check_bars_match_table(res, "hb_cat", arms)
    check_bars_match_table(res, "anemia", arms)
  }
  # missing_percent = "both" (two tables side by side): the bars count the
  # missing values, as the table's first percentages do
  res_both <- summarizeSCI(d, vars = c("hb", "hb_cat", "anemia"), group = "Arm",
                           missing_percent = "both", as_flex_table = FALSE)
  expect_equal(vapply(res_both$plots, function(p) p$type, ""),
               c(hb = "boxplot", hb_cat = "stacked barplot",
                 anemia = "stacked barplot"))
  res <- summarizeSCI(d, vars = "hb_cat", group = "Arm", as_flex_table = FALSE)
  expect_equal(res_both$plots$hb_cat$numbers$counts, res$plots$hb_cat$numbers$counts)
  # without missing values in the table, none in the plot
  res <- summarizeSCI(d, vars = "hb_cat", group = "Arm", missing = FALSE,
                      as_flex_table = FALSE)
  expect_false("Missing" %in% dimnames(res$plots$hb_cat$numbers$counts)[[1]])
  check_bars_match_table(res, "hb_cat", arms)
})

test_that("ungrouped summaries plot one box and one bar", {
  d <- visit_fixture()
  res <- summarizeSCI(d, vars = c("hb", "hb_cat"),
                      as_flex_table = FALSE)
  expect_equal(dimnames(res$plots$hb_cat$numbers$counts)[[2]], "All")
  expect_no_error(draw_quietly(print(res$plots$hb)))
})

test_that("plot_args reach the plots that have them", {
  d <- visit_fixture()
  res <- summarizeSCI(d, vars = c("hb", "hb_cat"), group = "Arm",
                      as_flex_table = FALSE,
                      plot_args = list(col = c("grey40", "grey80"), scale = "count"))
  expect_equal(res$plots$hb_cat$numbers$counts,
               summarizeSCI(d, vars = "hb_cat", group = "Arm",
                            as_flex_table = FALSE)$plots$hb_cat$numbers$counts)
  expect_warning(summarizeSCI(d, vars = "hb", group = "Arm",
                              as_flex_table = FALSE,
                              plot_args = list(nonsense = 1)),
                 "not used by any plot: nonsense")
  expect_error(summarizeSCI(d, vars = "hb", plot_args = list(1)),
               "named list")
})

test_that("plots are saved to files", {
  d <- visit_fixture()
  res <- summarizeSCI(d, vars = c("hb", "hb_cat"), group = "Arm",
                      as_flex_table = FALSE)
  dir <- file.path(tempdir(), "summarizeSCI-plots")
  on.exit(unlink(dir, recursive = TRUE))

  # default names: one file per variable
  files <- save_plots(res, file_path = dir, width = 6, height = 4)
  expect_equal(files, file.path(dir, c("hb.png", "hb_cat.png")))
  expect_true(all(file.exists(files)))
  # a single name is a prefix; one name per plot is used as given
  files <- save_plots(res, file_name = "fig", file_path = dir, format = "pdf")
  expect_equal(basename(files), c("fig_hb.pdf", "fig_hb_cat.pdf"))
  files <- save_plots(res, file_name = c("box", "bars.png"), file_path = dir,
                      format = "svg")
  expect_equal(basename(files), c("box.svg", "bars.svg"))
  expect_true(all(file.exists(files)))
  # a single plot
  files <- save_plots(res$plots$hb, file_name = "hemoglobin", file_path = dir)
  expect_equal(basename(files), "hemoglobin.png")
  expect_true(file.exists(files))

  expect_error(save_plots(res, file_name = c("a", "b", "c"), file_path = dir),
               "one entry per plot")
  expect_error(save_plots(res, file_path = dir, format = "doc"), "Unsupported")
  expect_error(save_plots(summarizeSCI(d, vars = "hb", type = "table")),
               "no plots")
})

test_that("informative errors", {
  d <- visit_fixture()
  expect_error(summarizeSCI(), "'data' must be specified")
  expect_error(summarizeSCI(d, vars = "hb", type = "plot", word_output = TRUE),
               "'word_output' saves the table")
})

test_that("word_output saves the table as summaryTable() does", {
  d <- visit_fixture()
  f  <- tempfile(fileext = ".docx")
  f2 <- tempfile(fileext = ".docx")
  on.exit(unlink(c(f, f2)))
  expect_message(res <- summarizeSCI(d, vars = "hb", group = "Arm",
                                     word_output = TRUE, file_path = f),
                 "Table saved to")
  expect_true(file.exists(f))
  expect_s3_class(res$table, "flextable")
  expect_length(res$plots, 1)
  # the same table as summaryTable() writes
  doc_text <- function(path) officer::docx_summary(officer::read_docx(path))$text
  suppressMessages(summaryTable(d, vars = "hb", group = "Arm",
                                word_output = TRUE, file_path = f2))
  expect_equal(doc_text(f), doc_text(f2))
  # also when the result holds the gtsummary table
  res <- suppressMessages(summarizeSCI(d, vars = "hb", group = "Arm",
                                       as_flex_table = FALSE,
                                       word_output = TRUE, file_path = f))
  expect_s3_class(res$table, "gtsummary")
})

test_that("the boxplot shows the quartiles and N of a median_IQR table", {
  d <- visit_fixture(1)
  d$hb[d$Arm == "Arm A"][1:3] <- NA
  res <- summarizeSCI(d, vars = "hb", group = "Arm", stat_cont = "median_IQR",
                      as_flex_table = FALSE, plot_args = list(risk_table = TRUE))
  tb <- as.data.frame(res$table$table_body)
  z  <- boxes_drawn(res$plots$hb)[[1]]
  for (g in 1:2) {
    cellv <- tb[tb$variable == "hb" & tb$row_type == "label", paste0("stat_", g)]
    num   <- parse_number(strsplit(gsub("[(),]", " ", cellv), " +")[[1]])
    expect_within_rounding(z$stats[c(3, 2, 4), g], num, digits = 1)
    n_tab <- parse_number(tb[tb$variable == "hb" & tb$row_type == "label",
                             paste0("add_n_stat_", g)])
    expect_equal(z$n[g], n_tab)
  }
})

test_that("groups the table leaves out are not plotted", {
  d <- visit_fixture()
  d$Arm <- factor(d$Arm, levels = c("Arm A", "Arm B", "Arm C"))   # Arm C empty
  d$Arm[1:2] <- NA                                                # no group
  res <- suppressMessages(summarizeSCI(d, vars = c("hb", "hb_cat"), group = "Arm",
                                       as_flex_table = FALSE))
  expect_equal(dimnames(res$plots$hb_cat$numbers$counts)[[2]], c("Arm A", "Arm B"))
  check_bars_match_table(res, "hb_cat", c("Arm A" = "Arm A", "Arm B" = "Arm B"))
  z <- boxes_drawn(res$plots$hb)[[1]]
  expect_equal(ncol(z$stats), 2)
  tb <- as.data.frame(res$table$table_body)
  expect_equal(z$n, parse_number(unlist(tb[tb$variable == "hb" & tb$row_type == "label",
                                           c("add_n_stat_1", "add_n_stat_2")])))
  leg <- drawn_calls(print(res$plots$hb), "C_text")
  expect_false("Arm C" %in% unlist(lapply(leg, function(a) Filter(is.character, a))))
})

