#' Plot a summaryByVisit table
#'
#' Plots one variable of a table produced by [summaryByVisit()] over the
#' visits, using the numbers printed in the table rather than the raw data.
#' Continuous variables are drawn as a line plot of the point estimate with
#' its interval (see [lineplotSCI()]), categorical variables as stacked bars
#' (see [stackedbarplotSCI()]).
#'
#' The statistic of continuous variables is read from the table footnote and
#' determines the error bars:
#' * `"median_range"`: median with whiskers from minimum to maximum;
#' * `"median_IQR"`: median with whiskers from the first to the third quartile;
#' * `"mean_sd"` / `"mean_se"`: mean +/- one standard deviation / standard error;
#' * `"geomMean_sd"`: geometric mean only, since a standard deviation on the
#'   original scale gives no meaningful interval around it.
#'
#' Since the numbers are read from the printed table, they carry its rounding
#' (`digits_cont` / `digits_cat`). Visits appear on the x-axis in the order
#' of the table rows.
#'
#' @param tbl A table returned by [summaryByVisit()] with
#' `as_flex_table = FALSE`.
#' @param var The variable to be plotted, given as its label in the table (the
#' bold header row, e.g. `"Hemoglobin"`) or its position among the variables of
#' the table. May be omitted if the table holds a single variable.
#' @param stat_cont The statistic of continuous variables, as passed to
#' [summaryByVisit()]. Default is `NULL`, which reads it from the table footnote.
#' @param missing A logical toggle switch adding the missing-value rows of a
#' categorical variable as a segment of their own. Default is `FALSE`, which
#' leaves them out, so that percentages are relative to the non-missing
#' observations.
#' @param missing_text The label of the missing-value rows in the table, as
#' passed to [summaryByVisit()]. Default is `"Missing"`.
#' @param ... Further arguments controlling the appearance of the plot, as
#' documented in [lineplotSCI()] (continuous variables; e.g. `interval_type`,
#' `risk_table`, `col`, `legend_position`) or [stackedbarplotSCI()]
#' (categorical variables; e.g. `scale`, `percent_labels`, `risk_table`).
#'
#' @return Invisibly, a list with
#' * `type`: `"continuous"` or `"categorical"`;
#' * `summary`: the rows of [tidySummaryByVisit()] belonging to `var`;
#' * for continuous variables, the group-by-visit matrices `centre`, `lower`,
#'   `upper` and `n`;
#' * for categorical variables, the arrays `counts` and `percent` indexed by
#'   level, group and visit, and the group-by-visit matrix `n` of denominators.
#'
#' @examples
#' library(summarySCI)
#' data("hemoglobin_data")
#' hemoglobin_data$hb_cat <- cut(hemoglobin_data$hb, c(-Inf, 120, 140, Inf),
#'                               labels = c("< 120", "120-140", "> 140"))
#' tbl <- summaryByVisit(hemoglobin_data, vars = c("hb", "hb_cat"),
#'                       group = "Arm", visit = "visit", stat_cont = "median_IQR")
#' plotByVisit(tbl, var = "Hemoglobin", risk_table = TRUE)
#' plotByVisit(tbl, var = "hb_cat", legend_title = "Hemoglobin")
#'
#' @export
plotByVisit <- function(tbl, var = NULL, stat_cont = NULL,
                        missing = FALSE, missing_text = "Missing", ...) {

  sm   <- tidySummaryByVisit(tbl, stat_cont = stat_cont)
  vars <- unique(sm$variable)

  if (is.null(var)) {
    if (length(vars) > 1L)
      stop("The table holds several variables; choose one with 'var': ",
           paste0('"', vars, '"', collapse = ", "), ".")
    var <- vars
  } else if (is.numeric(var)) {
    if (length(var) != 1L || !var %in% seq_along(vars))
      stop("'var' must be between 1 and ", length(vars), ".")
    var <- vars[var]
  } else if (length(var) != 1L || !var %in% vars) {
    stop("'var' not found in the table. Available: ",
         paste0('"', vars, '"', collapse = ", "), ".")
  }

  sm     <- sm[sm$variable == var, , drop = FALSE]
  type   <- sm$var_type[1]
  visits <- unique(sm$visit[order(sm$visit_id)])
  groups <- unique(sm$group)
  dims   <- list(groups, visits)

  args <- list(...)
  if (is.null(args$xlab)) args$xlab <- "Visit"

  if (type == "continuous") {
    centre <- lower <- upper <- matrix(NA_real_, length(groups), length(visits),
                                       dimnames = dims)
    obs <- matrix(0, length(groups), length(visits), dimnames = dims)
    idx <- cbind(match(sm$group, groups), match(sm$visit, visits))
    centre[idx] <- sm$centre
    lower[idx]  <- sm$lower
    upper[idx]  <- sm$upper
    obs[idx]    <- sm$n

    if (is.null(args$ylab)) args$ylab <- var
    xs  <- seq_along(visits)
    res <- do.call(lineplot_draw,
                   c(list(centre = centre, lower = lower, upper = upper,
                          obs = obs, xs = xs, at_x = xs, visit = visits),
                     args))
  } else {
    if (!isTRUE(missing)) sm <- sm[sm$level != missing_text, , drop = FALSE]
    levs   <- unique(sm$level)
    counts <- array(0, dim = c(length(levs), length(groups), length(visits)),
                    dimnames = list(levs, groups, visits))
    counts[cbind(match(sm$level, levs), match(sm$group, groups),
                 match(sm$visit, visits))] <- sm$count
    counts[is.na(counts)] <- 0
    n <- apply(counts, c(2, 3), sum)

    if (is.null(args$legend_title)) args$legend_title <- var
    res <- do.call(stackedbar_draw, c(list(counts = counts, n = n), args))
  }

  invisible(c(list(type = type, summary = sm), res))
}

#' Read the numbers off a summaryByVisit table
#'
#' Converts a table produced by [summaryByVisit()] into a data frame with one
#' row per variable, visit, group and (for categorical variables) level,
#' holding the parsed numbers. This is the parser behind [plotByVisit()].
#'
#' @inheritParams plotByVisit
#'
#' @return A data frame with columns `variable` (the label of the header row),
#' `var_type`, `visitgroup`, `visit`, `visit_id` (position of the visit within
#' the variable), `group`, `level` (`NA` for continuous variables), `stat`
#' (the cell as printed), `n` (the number of non-missing observations shown in
#' the table's N column), and the parsed numbers `centre`, `lower`, `upper`
#' (continuous) and `count` (categorical).
#'
#' @export
tidySummaryByVisit <- function(tbl, stat_cont = NULL) {

  if (inherits(tbl, "flextable"))
    stop("'tbl' is a flextable; create it with summaryByVisit(..., as_flex_table = FALSE).")
  if (!inherits(tbl, "gtsummary") || is.null(tbl$table_body))
    stop("'tbl' must be a table returned by summaryByVisit().")

  tb  <- as.data.frame(tbl$table_body)
  hdr <- tbl$table_styling$header

  # --- Columns: visible group columns, else the overall column --------------
  stat_cols <- hdr$column[grepl("^stat_[0-9]+$", hdr$column) & !hdr$hide]
  if (length(stat_cols) == 0L) stat_cols <- "stat_0"
  stat_cols <- stat_cols[order(stat_cols == "stat_0")]   # overall last
  n_cols <- ifelse(stat_cols == "stat_0", "n",
                   sub("^stat_", "add_n_stat_", stat_cols))
  group_lab <- hdr$label[match(stat_cols, hdr$column)]
  group_lab <- trimws(sub("\\n.*$", "", gsub("\\*\\*", "", group_lab)))
  group_lab[grepl("^N = ", group_lab) | group_lab == ""] <- "Overall"

  stat_type <- detect_stat_cont(tbl, stat_cont)

  # --- Row roles -------------------------------------------------------------
  # Header rows (row_type NA) are the variable label and, with 'visitgroup',
  # the visit group; visit rows have row_type "label" and level rows "level".
  # With visit groups every variable header is directly followed by a
  # visit-group header, so two header rows in a row mark a new variable.
  rt     <- tb$row_type
  is_hdr <- is.na(rt)
  nr     <- nrow(tb)
  has_vg <- nr > 1 && any(is_hdr[-1] & is_hdr[-nr])

  out <- list()
  cur_var <- cur_vg <- cur_visit <- NA_character_
  cur_type <- NA_character_
  visit_id <- 0L
  cur_n <- rep(NA_real_, length(stat_cols))

  for (i in seq_len(nr)) {
    lab <- tb$label[i]
    if (is_hdr[i]) {
      if (!has_vg || (i < nr && is_hdr[i + 1])) {
        cur_var  <- lab
        cur_vg   <- NA_character_
        visit_id <- 0L
      } else {
        cur_vg <- lab
      }
      next
    }

    vals <- unlist(tb[i, stat_cols], use.names = FALSE)
    if (rt[i] == "label") {
      cur_visit <- lab
      visit_id  <- visit_id + 1L
      cur_n     <- parse_number(unlist(tb[i, n_cols], use.names = FALSE))
      cur_type  <- if (all(is.na(vals))) "categorical" else "continuous"
      if (cur_type == "categorical") next
      est   <- parse_continuous(vals, stat_type)
      level <- NA_character_
      count <- NA_real_
    } else {
      est   <- list(centre = NA_real_, lower = NA_real_, upper = NA_real_)
      level <- lab
      count <- parse_count(vals)
    }

    out[[length(out) + 1L]] <- data.frame(
      variable = cur_var, var_type = cur_type, visitgroup = cur_vg,
      visit = cur_visit, visit_id = visit_id, group = group_lab,
      level = level, stat = vals, n = cur_n,
      centre = est$centre, lower = est$lower, upper = est$upper,
      count = count, stringsAsFactors = FALSE)
  }

  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# --- Parsing helpers ---------------------------------------------------------

# Numbers as printed by gtsummary: thousands separators, unicode minus, "NA"
parse_number <- function(x) {
  x <- gsub("−", "-", trimws(as.character(x)))
  suppressWarnings(as.numeric(gsub(",", "", x)))
}

# Leading count of "n (p%)", "n/N" and "n"
parse_count <- function(x) {
  parse_number(sub("[ /(].*$", "", trimws(x)))
}

# "c (a, b)" or "c (s)" -> centre and interval bounds
parse_continuous <- function(x, stat_type) {
  x      <- trimws(x)
  centre <- parse_number(sub("\\s*\\(.*$", "", x))
  inner  <- ifelse(grepl("\\(.*\\)", x), sub("^[^(]*\\((.*)\\)\\s*$", "\\1", x), NA)
  # ", " separates the bounds; a comma without a space is a thousands separator
  parts  <- strsplit(inner, ",\\s+")
  first  <- parse_number(vapply(parts, `[`, "", 1L))
  second <- parse_number(vapply(parts, function(p) if (length(p) > 1) p[2] else NA_character_, ""))

  switch(stat_type,
         median_range = ,
         median_IQR   = list(centre = centre, lower = first, upper = second),
         mean_sd      = ,
         mean_se      = list(centre = centre, lower = centre - first,
                             upper = centre + first),
         geomMean_sd  = list(centre = centre, lower = rep(NA_real_, length(x)),
                             upper = rep(NA_real_, length(x))))
}

# The statistic of continuous variables, read from the footnotes gtsummary
# attaches to the stat columns
detect_stat_cont <- function(tbl, stat_cont = NULL) {
  known <- c("median_range", "median_IQR", "mean_sd", "mean_se", "geomMean_sd")
  if (!is.null(stat_cont)) {
    if (!stat_cont %in% known)
      stop("'stat_cont' must be one of ", paste0('"', known, '"', collapse = ", "), ".")
    return(stat_cont)
  }
  fn <- unique(tolower(tbl$table_styling$footnote_header$footnote))
  fn <- fn[!is.na(fn)]
  found <- unique(c(
    if (any(grepl("geom", fn)))                                  "geomMean_sd",
    if (any(grepl("^median.*q1", fn)))                           "median_IQR",
    if (any(grepl("^median.*min", fn)))                          "median_range",
    if (any(grepl("^mean \\(sd\\)", fn)))                        "mean_sd",
    if (any(grepl("^mean \\(se\\)", fn)))                        "mean_se"))
  if (length(found) > 1L)
    stop("Several continuous statistics found in the table footnotes; ",
         "set 'stat_cont'.")
  if (length(found) == 0L) "median_range" else found
}
