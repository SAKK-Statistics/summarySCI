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
#' * `"geomMean_sd"`: geometric mean divided and multiplied by the geometric
#'   standard deviation.
#'
#' Since the numbers are read from the printed table, they carry its rounding
#' (`digits_cont` / `digits_cat`). Visits appear on the x-axis in the order
#' of the table rows.
#'
#' @param tbl A table returned by [summaryByVisit()] with
#' `as_flex_table = FALSE` (the default `TRUE` returns a flextable, from which
#' the numbers cannot be read back).
#' @param var The variable to be plotted, given as its label in the table (the
#' bold header row, e.g. `"Hemoglobin"`) or its position among the variables of
#' the table. May be omitted if the table holds a single variable.
#' @param stat_cont The statistic of continuous variables, as passed to
#' [summaryByVisit()]. Default is `NULL`, which reads it from the table
#' footnote; if the footnote is not recognised, `plotByVisit()` stops and asks
#' for `stat_cont` rather than guess. A `stat_cont` that does not fit the
#' printed cells (e.g. `"mean_sd"` for a median (range) table) is an error too.
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
#' For continuous variables, `xs` sets the positions of the visits on the
#' x-axis (one number per visit, e.g. the study days); by default they are
#' evenly spaced.
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
#' hb <- subset(hemoglobin_data, visit_nr <= 3)   # first 3 cycles, to keep it short
#' attr(hb$hb, "label") <- "Hemoglobin"           # subset() drops the label
#' tbl <- summaryByVisit(hb, vars = "hb",
#'                       group = "Arm", visit = "visit", stat_cont = "median_IQR",
#'                       as_flex_table = FALSE)
#' plotByVisit(tbl, var = "Hemoglobin", risk_table = TRUE)
#'
#' \donttest{
#' # a categorical variable
#' hb$hb_cat <- cut(hb$hb, c(-Inf, 120, 140, Inf),
#'                  labels = c("< 120", "120-140", "> 140"))
#' tbl <- summaryByVisit(hb, vars = c("hb", "hb_cat"),
#'                       group = "Arm", visit = "visit", stat_cont = "median_IQR",
#'                       as_flex_table = FALSE)
#' plotByVisit(tbl, var = "hb_cat", legend_title = "Hemoglobin")
#' }
#'
#' @export
plotByVisit <- function(tbl, var = NULL, stat_cont = NULL,
                        missing = FALSE, missing_text = "Missing", ...) {

  sm   <- parse_summary_by_visit(tbl, stat_cont = stat_cont, strict = FALSE)
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
  if (type == "continuous" && is.na(detect_stat_cont(tbl, stat_cont)))
    stop_unknown_stat()
  visits <- unique(sm$visit[order(sm$visit_id)])
  groups <- unique(sm$group)
  dims   <- list(groups, visits)

  args <- list(...)
  if (is.null(args[["xlab"]])) args[["xlab"]] <- "Visit"
  # x positions of the visits (summarizeByVisitSCI() passes those of 'order')
  xs <- if (is.null(args[["xs"]])) seq_along(visits) else args[["xs"]]
  args[["xs"]] <- NULL
  if (!is.numeric(xs) || length(xs) != length(visits) || anyNA(xs))
    stop("'xs' must give one x position per visit (", length(visits), ").")

  if (type == "continuous") {
    centre <- lower <- upper <- matrix(NA_real_, length(groups), length(visits),
                                       dimnames = dims)
    obs <- matrix(0, length(groups), length(visits), dimnames = dims)
    idx <- cbind(match(sm$group, groups), match(sm$visit, visits))
    centre[idx] <- sm$centre
    lower[idx]  <- sm$lower
    upper[idx]  <- sm$upper
    obs[idx]    <- sm$n

    if (is.null(args[["ylab"]])) args[["ylab"]] <- var
    st <- stat_cont_plot(detect_stat_cont(tbl, stat_cont))
    args[["stat_footnote"]] <- footnote_text(args[["stat_footnote"]],
                                             line_note(st[1], st[2]))
    res <- do.call(lineplot_draw,
                   c(list(centre = centre, lower = lower, upper = upper,
                          obs = obs, xs = xs, at_x = xs, visit = visits),
                     args))
  } else {
    if (!isTRUE(missing)) sm <- sm[sm$level != missing_text, , drop = FALSE]
    # the missing-value segment on top of the stack, hatched
    levs   <- unique(sm$level)
    levs   <- c(setdiff(levs, missing_text), intersect(levs, missing_text))
    counts <- array(0, dim = c(length(levs), length(groups), length(visits)),
                    dimnames = list(levs, groups, visits))
    counts[cbind(match(sm$level, levs), match(sm$group, groups),
                 match(sm$visit, visits))] <- sm$count
    counts[is.na(counts)] <- 0
    n <- apply(counts, c(2, 3), sum)

    if (is.null(args[["legend_title"]])) args[["legend_title"]] <- var
    if (is.null(args[["missing_seg"]]) && missing_text %in% levs)
      args[["missing_seg"]] <- match(missing_text, levs)
    args[["stat_footnote"]] <- footnote_text(
      args[["stat_footnote"]],
      bar_note(if (is.null(args[["scale"]])) "percent" else args[["scale"]],
               missing_shown = !is.null(args[["missing_seg"]])))
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
  parse_summary_by_visit(tbl, stat_cont = stat_cont, strict = TRUE)
}

# The parser. With strict = FALSE, continuous rows whose statistic cannot be
# told are returned with NA numbers instead of stopping, so that plotByVisit()
# can still plot the categorical variables of the same table.
parse_summary_by_visit <- function(tbl, stat_cont = NULL, strict = TRUE) {

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
      if (is.na(stat_type)) {
        if (strict) stop_unknown_stat()
        est <- list(centre = NA_real_, lower = NA_real_, upper = NA_real_)
      } else {
        est <- parse_continuous(vals, stat_type)
      }
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
  x <- gsub("\u2212", "-", trimws(as.character(x)))
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

  # The cells must have the shape of the statistic: two values in brackets for
  # median (range / IQR), one for mean (SD / SE) and geometric mean (SD)
  expected <- if (stat_type %in% c("median_range", "median_IQR")) 2L else 1L
  bad <- !is.na(inner) & lengths(parts) != expected
  if (any(bad))
    stop("Cell \"", x[bad][1], "\" does not have the shape of '", stat_type,
         "'; set 'stat_cont' to the value passed to summaryByVisit().")

  switch(stat_type,
         median_range = ,
         median_IQR   = list(centre = centre, lower = first, upper = second),
         mean_sd      = ,
         mean_se      = list(centre = centre, lower = centre - first,
                             upper = centre + first),
         geomMean_sd  = list(centre = centre, lower = centre / first,
                             upper = centre * first))
}

stop_unknown_stat <- function() {
  stop("Cannot tell from the table footnotes which statistic the continuous ",
       "rows show; set 'stat_cont' to the value passed to summaryByVisit().",
       call. = FALSE)
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
  # summaryByVisit joins the footnotes of several variables with "; "
  fn <- tolower(tbl$table_styling$footnote_header$footnote)
  fn <- unique(trimws(unlist(strsplit(fn[!is.na(fn)], ";"))))
  found <- unique(c(
    if (any(grepl("geom", fn)))                                  "geomMean_sd",
    if (any(grepl("^median.*q1", fn)))                           "median_IQR",
    if (any(grepl("^median.*min", fn)))                          "median_range",
    if (any(grepl("^mean \\(sd\\)", fn)))                        "mean_sd",
    if (any(grepl("^mean \\(se\\)", fn)))                        "mean_se"))
  if (length(found) > 1L)
    stop("Several continuous statistics found in the table footnotes; ",
         "set 'stat_cont'.")
  # NA when not recognised: tidySummaryByVisit() stops at the first continuous
  # row rather than guess, while tables of categorical variables still parse
  if (length(found) == 0L) NA_character_ else found
}
