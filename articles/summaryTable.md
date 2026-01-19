# summaryTable

The function
[`summaryTable()`](https://sakk-statistics.github.io/summarySAKK/reference/summaryTable.md)
produces a table with descriptive statistics for continuous, categorical
and dichotomous variables. It is based on the function
[`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html),
with several enhancements and simplifications, such as

- Simplified syntax for easier and more intuitive use.
- Display of missing values for categorical variables: Option to show
  (or not) the percentage of missing values next to the count.
- Columns with the number of non-missing observations can be added for
  each group

### Setup and data

To demonstrate the various functionalities of the function we will use
the dataset
[`survival::colon`](https://rdrr.io/pkg/survival/man/colon.html).

``` r
library(survival)
data(cancer, package="survival")

colon1 <-  colon %>%
  group_by(id) %>%
  slice(1) %>% # Select the first row within each id group
  ungroup()
  
```

The dataset `colon` contains data of 1858 patients from one of the first
successful trials of adjuvant chemotherapy for colon cancer.

For simplicity, we focus here on recurrence only, two treatment groups,
and four variable:

- the treatment group (`rx`),
- the sex (`Male`),
- the age (`age`) and
- the extent of local spread (`extent`).

We also add a few missing values for the variable `extent`.

``` r
set.seed(123)
colon2 <- colon1 %>%
  select(rx, sex, age, extent) %>%
  filter(rx != "Lev") %>%
  mutate(rx = if_else(rx == "Obs", "Control", rx),
         extent = if_else(row_number() %in% sample(row_number(), size = round(0.1 * n())), NA, extent)) %>% 
  rename(Male = sex) %>% 
  mutate(extent = as.factor(extent))
```

``` r
head(colon2)
#> # A tibble: 6 × 4
#>   rx       Male   age extent
#>   <chr>   <dbl> <dbl> <fct> 
#> 1 Lev+5FU     1    43 3     
#> 2 Lev+5FU     1    63 3     
#> 3 Control     0    71 2     
#> 4 Lev+5FU     0    66 3     
#> 5 Control     1    69 3     
#> 6 Lev+5FU     0    57 3
```

### Simple table

By default, the function produces a table with all variables present in
the dataset.

``` r
summaryTable(data = colon2)
```

| Characteristic            | N   | N = 6191          |
|---------------------------|-----|-------------------|
| rx                        | 619 |                   |
| Control                   |     | 315 (51%)         |
| Lev+5FU                   |     | 304 (49%)         |
| Male                      | 619 |                   |
| 0                         |     | 312 (50%)         |
| 1                         |     | 307 (50%)         |
| age                       | 619 | 61.0 (18.0, 85.0) |
| extent                    | 557 |                   |
| 1                         |     | 17 (3%)           |
| 2                         |     | 68 (11%)          |
| 3                         |     | 446 (72%)         |
| 4                         |     | 26 (4%)           |
| Missing                   |     | 62 (10%)          |
| 1n (%); Median (Min, Max) |     |                   |

If only specific variables are to be included, they need to be entered
in the argument `vars`. The argument `group` allows the summary
statistics to be stratified by this variable.

``` r
summaryTable(data = colon2, 
             vars = c("Male", "age", "extent"), 
             group = "rx")
```

[TABLE]

#### Displayed name of variables

The displayed name of each variable is

- the label if it exists in the dataset, or

- the variable name if no label is present in the dataset (which is the
  case in our example).

In order to customize the displayed name, the argument `labels` can be
used. Please note that the labels need to be entered as a list, as shown
below:

``` r
summaryTable(data = colon2, 
             group = "rx",
             labels = list(age = "Age", extent = "Extent"))
```

[TABLE]

### Adding number of observations

The number of observations **which are not missing values** are by
default added in a new column. This can be disabled by setting the
argument `add_n` to `FALSE`.

``` r
summaryTable(data = colon2, 
             group = "rx",
            labels = list(rx = "Arm", age = "Age", extent = "Extent"), 
             add_n = FALSE)
```

[TABLE]

### Overall column

An “overall” column can be added by setting the argument `overall` to
`TRUE`.

``` r
summaryTable(data = colon2, 
             group = "rx",
             overall = TRUE, 
             labels = list(age = "Age", extent = "Extent"))
```

[TABLE]

### Variable types

The function
[`gtsummary::tbl_summary`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
considers numeric variables with fewer than 10 unique values as
categorical by default. This is not the case in the function
`summaryTable`.

Per default, all numeric variables are considered as continuous, unless
they only have two unique values: 0 and 1. In that case, they are
considered as dichotomous. This can be changed by setting the argument
`continuous_as` to `categorical`.

For dichotomous variables, all levels are displayed by default. To show
only one row, use the argument `dichotomous_as = dichotomous`. The
reference level is specified using the argument
`value = list(variable ~ "level to show")`.

``` r
summaryTable(data = colon2,
             group = "rx",
             vars = "Male",
            labels = list(age = "Age"), 
            dichotomous_as = "dichotomous", 
            value = list(Male ~ "1"),
            missing = FALSE)
```

[TABLE]

By default, the function plots the median and range for continuous
variables. A number of other options are available, using the argument
`stat_cont`.

#### Statistic type

The statistics to be displayed can be chosen using the argument
`stat_cont` (options: `median_IQR`, `median_range` (default),
`"mean_sd"`, `"mean_se"` and `"geomMean_sd"`) and `stat_cat` (options:
`"n_percent"` (default) `"n"` and `"n_N"`).

``` r
summaryTable(data = colon2, group = "rx", 
             stat_cont = "median_IQR", 
             stat_cat = "n_N",
              labels = list(age = "Age", sex = "Sex", extent = "Extent"))
```

[TABLE]

### Tests

By default, no p-value and confidence (CI) are displayed. p-values can
be added by setting `test` to `TRUE` and CI by setting `ci` to `TRUE`.

The default test type for continuous variable is `wilcox.test`, and
`fisher.test` for categorical variables. This can be changed in
`test_cont` and `test_cat`, respectively.

The default CI type for continuous variables is `wilcox.test` and
`wilson` for categorical variables. This can be changed in `ci_cont` and
`ci_cat`, respectively.

``` r
summaryTable(data = colon2, 
             group = "rx", 
             vars = c("age", "extent"), 
             stat_cont = "mean_sd", 
             test = TRUE,
             ci = TRUE,
             labels = list(age = "Age", extent = "Extent")
             )
#> The number rows in the tables to be merged do not match, which may result in
#> rows appearing out of order.
#> ℹ See `tbl_merge()` (`?gtsummary::tbl_merge()`) help file for details. Use
#>   `quiet=TRUE` to silence message.
```

[TABLE]

### Missing values

Per default, missing values are shown as a separate category. This can
be disabled by setting `missing` to `FALSE`.

For `missing = TRUE`, the percentage are automatically added next to the
missing number. This can be disabled by setting the argument
`missing_percentage` to `FALSE`.

``` r
summaryTable(data = colon2, 
             group = "rx", 
             vars = "extent", 
             test = TRUE,
             ci = TRUE,
             missing_percent = FALSE,
             labels = list(extent = "Extent")
             )
```

[TABLE]

``` r

summaryTable(data = colon2, 
             group = "rx", 
             vars = "extent", 
             test = TRUE,
             ci = TRUE,
             missing_percent = TRUE,
             labels = list(extent = "Extent")
             )
#> The number rows in the tables to be merged do not match, which may result in
#> rows appearing out of order.
#> ℹ See `tbl_merge()` (`?gtsummary::tbl_merge()`) help file for details. Use
#>   `quiet=TRUE` to silence message.
```

[TABLE]

The tables with and without missing values can also be put next to each
other by setting `missing` to `"both"`.

``` r
summaryTable(data = colon2, 
             group = "rx", 
             vars = "extent", 
             missing_percent = "both", 
             test = TRUE,
              labels = list(extent = "Extent")
             )
#> The number rows in the tables to be merged do not match, which may result in
#> rows appearing out of order.
#> ℹ See `tbl_merge()` (`?gtsummary::tbl_merge()`) help file for details. Use
#>   `quiet=TRUE` to silence message.
#> The number rows in the tables to be merged do not match, which may result in
#> rows appearing out of order.
#> ℹ See `tbl_merge()` (`?gtsummary::tbl_merge()`) help file for details. Use
#>   `quiet=TRUE` to silence message.
```

[TABLE]

### Further customization

Digits can be customized with the arguments `digits_cont` and
`digits_cat`. The argument `as_flex_table` (default to `TRUE`) converts
the gtsummary object to a flextable object, which is better for Word
output.

## Next steps

The argument `type` will be introduced in a future release to enable
more fine-grained customization of the variables types.
