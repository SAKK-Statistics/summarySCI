# Save plots to files

Writes the plots of a
[`summarizeSCI()`](https://sakk-statistics.github.io/summarySCI/reference/summarizeSCI.md)
or
[`summarizeByVisitSCI()`](https://sakk-statistics.github.io/summarySCI/reference/summarizeByVisitSCI.md)
result, or a single plot such as `res$plots$hb`, each into its own file.

## Usage

``` r
save_plots(
  x,
  file_name = NULL,
  file_path = ".",
  format = "png",
  width = 8,
  height = 5,
  res = 300
)
```

## Arguments

- x:

  The result of
  [`summarizeSCI()`](https://sakk-statistics.github.io/summarySCI/reference/summarizeSCI.md)
  or
  [`summarizeByVisitSCI()`](https://sakk-statistics.github.io/summarySCI/reference/summarizeByVisitSCI.md)
  (made with `type = "both"` or `"plot"`), or a single plot, e.g.
  `res$plots$hb`.

- file_name:

  Name(s) of the files, without extension. Either one name per plot, or
  a single name that prefixes the variable names when `x` holds several
  plots (e.g. `"fig"` gives `fig_hb.png`, `fig_hb_cat.png`). Default is
  `NULL`, which names each file after its variable (`hb.png`).

- file_path:

  Folder to write the files into. Created if it does not exist. Default
  is the working directory.

- format:

  File format: `"png"` (the default), `"jpeg"`, `"tiff"`, `"pdf"` or
  `"svg"`.

- width, height:

  Size of the plot in inches. Default is 8 by 5.

- res:

  Resolution in dots per inch, for the pixel formats. Default is 300.

## Value

The paths of the files, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
save_plots(res)                                   # hb.png, hb_cat.png
save_plots(res, file_name = "fig", file_path = "figures", format = "pdf")
save_plots(res$plots$hb, file_name = "hemoglobin")  # hemoglobin.png
} # }
```
