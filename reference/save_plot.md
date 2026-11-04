# Save a plot to a file

Draws a plot of
[`summarizeSCI()`](https://sakk-statistics.github.io/summarySCI/reference/summarizeSCI.md)
into a file. The format follows the file extension: `.png`,
`.jpeg`/`.jpg`, `.tiff`, `.pdf` or `.svg`.

## Usage

``` r
save_plot(x, file, width = 8, height = 5, res = 300)
```

## Arguments

- x:

  A `sci_plot` object, e.g. `res$plots$hb`.

- file:

  Path of the file to write.

- width, height:

  Size of the plot in inches. Default is 8 by 5.

- res:

  Resolution in dots per inch, for the pixel formats. Default is 300.

## Value

The path of the file, invisibly.
