# summaryLevels

The function
[`summaryLevels()`](https://sakk-statistics.github.io/summarySAKK/reference/summaryLevels.md)
produces a table with descriptive statistics for levels of a categorical
variable, when those are saved as binary variables in different columns.
It is largely based on the function
[`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html).
The changes as compared to `tbl_summary` are:

- Collapses binary variables from different columns into levels of a
  factor and creates a summary table.

### Setup and data

To demonstrate the various functionalities of the function, we will
create a small dataset. The factor of interest is ‘Site of progression’.
For each site of progression, presence or absence is decoded in a
separate column. In addition, we have a grouping variable called ‘arm’.

``` r
data<- as.data.frame(cbind(c(1:10), c("A","A","A","A","A","B","B","B","B","B"),
                            c("absent","present","absent","present","absent","absent","present","absent","present","absent"),
                            c("absent","absent","present","absent","absent","absent","absent","absent","absent","absent"),
                            c("present","absent","present","present","present","present","present","present","present","present")))
names(data)<-c("upn", "arm", "liver", "lung", "brain")
  
```

### Basic table

Now, we use
[`summarySCI::summaryLevels`](https://sakk-statistics.github.io/summarySAKK/reference/summaryLevels.md)
to collapse the columns ‘liver’, ‘lung’ and ‘brain’ into a single factor
named ‘Site of progression’. The presence of each site of progression is
decoded as ‘present’. We need to define the columns containing the
factor levels using the `vars` argument.

``` r
summarySCI::summaryLevels(data=data,
                      vars = c("liver", "lung", "brain"),
                      label = "Site of progression",
                      levels= "present",)
```

| Site of progression1          | N = 102 |
|-------------------------------|---------|
| liver                         | 4 (40%) |
| lung                          | 1 (10%) |
| brain                         | 9 (90%) |
| 1More than one entry possible |         |
| 2n (%)                        |         |

The footnote emphazises that a patient may have more than one site of
progression. Therefore, the column percentages do not neccessarily add
up to 100%.

### By group

We can stratify the table by groups via the `group` argument. The
overall column can still be shown if desired, using the `overall = TRUE`
argument.

``` r
summarySCI::summaryLevels(data=data,
                      vars = c("liver", "lung", "brain"),
                      group = "arm",
                      label = "Site of progression",
                      levels= "present")
```

[TABLE]

``` r
summarySCI::summaryLevels(data=data,
                      vars = c("liver", "lung", "brain"),
                      group = "arm",
                      label = "Site of progression",
                      levels= "present",
                      overall = TRUE)
```

[TABLE]

### Performing a statistical test

A statistical test can be performed for each level separately when the
argument `test` is set to `TRUE`. The type of test can be changed with
the `test_cat` argument. Options include `chisq.test`,
`chisq.test.no.correct`, `fisher.test` (default)

``` r
summarySCI::summaryLevels(data=data,
                      vars = c("liver", "lung", "brain"),
                      group = "arm",
                      label = "Site of progression",
                      test = TRUE,,
                      levels= "present")
```

[TABLE]

``` r
summarySCI::summaryLevels(data=data,
                      vars = c("liver", "lung", "brain"),
                      group = "arm",
                      levels= "present",
                      label = "Site of progression",
                      test = TRUE,
                      test_cat = "chisq.test")
#> The following warnings were returned during `add_p()`:
#> ! For variable `liver` (`arm`) and "statistic", "p.value", and "parameter"
#>   statistics: Chi-squared approximation may be incorrect
#> The following warnings were returned during `add_p()`:
#> ! For variable `lung` (`arm`) and "statistic", "p.value", and "parameter"
#>   statistics: Chi-squared approximation may be incorrect
#> The following warnings were returned during `add_p()`:
#> ! For variable `brain` (`arm`) and "statistic", "p.value", and "parameter"
#>   statistics: Chi-squared approximation may be incorrect
```

[TABLE]

### Add a confidence interval

A confidence interval can be added, if requested using the statement
`ci = TRUE`. The confidence level can be adjusted by `conf_level`. The
type of confidence interval can be chosen using the command `ci_cat`.

``` r
summarySCI::summaryLevels(data=data,
                      vars = c("liver", "lung", "brain"),
                      group = "arm",
                      label = "Site of progression",
                      levels= "present",
                      test = TRUE,
                      test_cat = "fisher.test",
                      ci=TRUE,
                      conf_level = 0.9,
                      overall = FALSE)
```

[TABLE]

## Further steps:

- make it possible to stack tables
