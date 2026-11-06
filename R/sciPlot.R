#' Plot objects
#'
#' The plotting functions of the package draw with base R graphics, which
#' produce no object to keep. The wrappers [summarizeSCI()] therefore store
#' each plot as a `sci_plot`: the instructions to draw it, together with the
#' numbers it shows. Printing a `sci_plot` draws it on the current device, and
#' [save_plot()] draws it into a file of the requested size, so that the
#' layout is always worked out for the size it is drawn at.
#'
#' @param x A `sci_plot` object.
#' @param ... Ignored.
#'
#' @return `print()` and `plot()` return `x` invisibly.
#' @name sci_plot
NULL

# Build a sci_plot calling `fun(data, <args>)`, where `data` is the data or
# (for plotByVisit()) the table. The column names in `args` are character
# strings, which all plotting functions accept; the data is referred to by
# name, so that error messages do not print the whole data frame. The plot is
# drawn once on a null device, so that a problem shows at once rather than
# when the plot is printed, and to record its numbers.
new_sci_plot <- function(fun, fun_name, data, args, variable) {
  cl  <- as.call(c(list(fun, quote(data)), args))
  env <- new.env(parent = emptyenv())
  env$data <- data
  draw <- function() eval(cl, env)

  old <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  on.exit({
    grDevices::dev.off()
    if (old > 1) grDevices::dev.set(old)
  })
  numbers <- draw()

  structure(list(draw = draw, type = fun_name, variable = variable,
                 numbers = numbers),
            class = "sci_plot")
}

#' @rdname sci_plot
#' @export
print.sci_plot <- function(x, ...) {
  x$draw()
  invisible(x)
}

#' @rdname sci_plot
#' @export
plot.sci_plot <- function(x, ...) {
  print(x)
}

#' Save a plot to a file
#'
#' Draws a plot of [summarizeSCI()] into a file. The format follows the file
#' extension: `.png`, `.jpeg`/`.jpg`, `.tiff`, `.pdf` or `.svg`.
#'
#' @param x A `sci_plot` object, e.g. `res$plots$hb`.
#' @param file Path of the file to write.
#' @param width,height Size of the plot in inches. Default is 8 by 5.
#' @param res Resolution in dots per inch, for the pixel formats. Default is 300.
#'
#' @return The path of the file, invisibly.
#' @export
save_plot <- function(x, file, width = 8, height = 5, res = 300) {
  if (!inherits(x, "sci_plot"))
    stop("'x' must be a plot of summarizeSCI(), e.g. res$plots$hb.")
  ext <- tolower(tools::file_ext(file))
  old <- grDevices::dev.cur()
  switch(ext,
         png  = grDevices::png(file, width, height, units = "in", res = res),
         jpg  = ,
         jpeg = grDevices::jpeg(file, width, height, units = "in", res = res),
         tiff = grDevices::tiff(file, width, height, units = "in", res = res),
         pdf  = grDevices::pdf(file, width, height),
         svg  = grDevices::svg(file, width, height),
         stop("Unsupported file type '.", ext,
              "'; use .png, .jpeg, .tiff, .pdf or .svg."))
  on.exit({
    grDevices::dev.off()
    if (old > 1) grDevices::dev.set(old)
  })
  x$draw()
  invisible(file)
}

#' Save plots to files
#'
#' Writes the plots of a [summarizeSCI()] or [summarizeByVisitSCI()] result,
#' or a single plot such as `res$plots$hb`, each into its own file.
#'
#' @param x The result of [summarizeSCI()] or [summarizeByVisitSCI()] (made
#' with `type = "both"` or `"plot"`), or a single plot, e.g. `res$plots$hb`.
#' @param file_name Name(s) of the files, without extension. Either one name
#' per plot, or a single name that prefixes the variable names when `x` holds
#' several plots (e.g. `"fig"` gives `fig_hb.png`, `fig_hb_cat.png`). Default
#' is `NULL`, which names each file after its variable (`hb.png`).
#' @param file_path Folder to write the files into. Created if it does not
#' exist. Default is the working directory.
#' @param format File format: `"png"` (the default), `"jpeg"`, `"tiff"`,
#' `"pdf"` or `"svg"`.
#' @inheritParams save_plot
#'
#' @return The paths of the files, invisibly.
#'
#' @examples
#' \dontrun{
#' save_plots(res)                                   # hb.png, hb_cat.png
#' save_plots(res, file_name = "fig", file_path = "figures", format = "pdf")
#' save_plots(res$plots$hb, file_name = "hemoglobin")  # hemoglobin.png
#' }
#' @export
save_plots <- function(x, file_name = NULL, file_path = ".", format = "png",
                       width = 8, height = 5, res = 300) {
  plots <- if (inherits(x, "sci_plot")) {
    stats::setNames(list(x), x$variable)
  } else {
    x$plots
  }
  if (length(plots) == 0L)
    stop("'x' has no plots; create it with type = \"both\" or \"plot\".")
  format <- tolower(sub("^\\.", "", format))

  vars <- names(plots)
  file_name <- if (is.null(file_name)) {
    vars
  } else if (length(file_name) == length(plots)) {
    file_name
  } else if (length(file_name) == 1L) {
    paste0(file_name, "_", vars)
  } else {
    stop("'file_name' must have one entry per plot (", length(plots),
         ") or a single entry.")
  }
  # an image extension given in file_name is replaced by 'format' (only those,
  # since variable names may contain dots)
  file_name <- sub("\\.(png|jpe?g|tiff?|pdf|svg)$", "", file_name,
                   ignore.case = TRUE)

  dir.create(file_path, showWarnings = FALSE, recursive = TRUE)
  files <- file.path(file_path, paste0(file_name, ".", format))
  for (i in seq_along(plots)) {
    save_plot(plots[[i]], files[i], width = width, height = height, res = res)
  }
  invisible(files)
}

# Build one sci_plot per variable from `specs`, a named list (by variable) of
# list(fun, name, args, accepts, data); `data`, if given, replaces the common
# data for that plot. `plot_args` override the defaults, each plot taking
# those in `accepts` (by default the arguments of `fun`); arguments no plot
# uses give a warning.
build_plots <- function(specs, data, plot_args) {
  plots <- list()
  used  <- character()
  for (v in names(specs)) {
    s    <- specs[[v]]
    args <- s$args[!vapply(s$args, is.null, logical(1))]
    accepts <- if (is.null(s$accepts)) names(formals(s$fun)) else s$accepts
    fits <- intersect(names(plot_args), setdiff(accepts, "..."))
    args[fits] <- plot_args[fits]
    used <- c(used, fits)
    plots[[v]] <- new_sci_plot(s$fun, s$name,
                               if (is.null(s$data)) data else s$data, args, v)
  }
  unused <- setdiff(names(plot_args), used)
  if (length(unused) > 0)
    warning("'plot_args' not used by any plot: ",
            paste(unused, collapse = ", "), ".", call. = FALSE)
  plots
}

# Argument checks shared by the wrappers
check_wrapper_args <- function(plot_args, type, word_output) {
  if (isTRUE(word_output) && type == "plot")
    stop("'word_output' saves the table; use type = \"both\" or \"table\".",
         call. = FALSE)
  if (!is.list(plot_args) || (length(plot_args) > 0 && is.null(names(plot_args))))
    stop("'plot_args' must be a named list, e.g. list(risk_table = TRUE).",
         call. = FALSE)
}

# Stacked barplot of a categorical variable, set up like its table: the label
# titles the legend, stat_cat = "n" shows counts, and missing values form a
# segment of their own exactly when the table's percentages count them.
bar_spec <- function(v, label, group, stat_cat, missing, missing_percent,
                     missing_text, order = NULL, visit = NULL, xlab = NULL) {
  list(fun = stackedbarplotSCI, name = "stacked barplot",
       args = list(var = v, order = order, visit = visit, group = group,
                   legend_title = label, xlab = xlab,
                   scale = if (stat_cat == "n") "count" else "percent",
                   missing = isTRUE(missing) && !isFALSE(missing_percent),
                   missing_text = missing_text))
}

# The object both wrappers return: the table (as flextable if requested;
# NULL when only the plots were asked for) and the plots. With 'word_output',
# the table is saved as summaryTable() / summaryByVisit() save it.
wrapper_result <- function(tbl, plots, type, as_flex_table, border, class,
                           word_output = FALSE, file_path = NULL) {
  ft <- if (type != "plot" && (isTRUE(as_flex_table) || isTRUE(word_output)))
    to_flextable(tbl, border)
  if (isTRUE(word_output)) save_word(ft, file_path)
  table <- if (type == "plot") NULL else
    if (isTRUE(as_flex_table)) ft else tbl
  structure(list(table = table, plots = plots), class = class)
}
