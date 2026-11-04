# Resolve a column-name argument

Turns the argument of a plotting function into column name(s). Accepts
quoted names (`"x"`, `c("x", "y")`), a variable holding such names, and
bare names (`x`): a bare name does not evaluate to a character vector,
so it is deparsed instead.

## Usage

``` r
col_names(sub, val_fun)
```

## Arguments

- sub:

  The unevaluated argument, i.e. `substitute(arg)`.

- val_fun:

  A function evaluating the argument, i.e. `function() arg`.

## Value

A character vector, or `NULL` if the argument is `NULL`.
