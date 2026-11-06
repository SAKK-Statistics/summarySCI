# Extracted from test-summarizeSCI.R:99

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "summarySCI", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran()
visit_fixture <- function(v = 2) {
  d <- hb_fixture()
  d <- d[d$visit_nr == v, ]
  d$anemia <- ifelse(d$hb < 120, "Yes", "No")
  d
}
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

# test -------------------------------------------------------------------------
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
res <- summarizeSCI(d, vars = "hb_cat", group = "Arm", missing_percent = "both",
                      as_flex_table = FALSE)
