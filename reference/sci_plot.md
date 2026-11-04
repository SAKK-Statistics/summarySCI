# Plot objects

The plotting functions of the package draw with base R graphics, which
produce no object to keep. The wrappers
[`summarizeSCI()`](https://sakk-statistics.github.io/summarySCI/reference/summarizeSCI.md)
therefore store each plot as a `sci_plot`: the instructions to draw it,
together with the numbers it shows. Printing a `sci_plot` draws it on
the current device, and
[`save_plot()`](https://sakk-statistics.github.io/summarySCI/reference/save_plot.md)
draws it into a file of the requested size, so that the layout is always
worked out for the size it is drawn at.

## Usage

``` r
# S3 method for class 'sci_plot'
print(x, ...)

# S3 method for class 'sci_plot'
plot(x, ...)
```

## Arguments

- x:

  A `sci_plot` object.

- ...:

  Ignored.

## Value

[`print()`](https://rdrr.io/r/base/print.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) return `x`
invisibly.
