# summaryByVisit

The function
[`summaryByVisit()`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
produces a table with descriptive statistics for continues and
categorical variables at different time points (visits). It is largely
based on the function
[`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
and
[`gtsummary::tbl_strata_nested_stack`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_strata_nested_stack.html).
The changes as compared to these functions are:

- A hierarchical summary table is created by variable (lab-value), visit
  and visitgroup.

### Setup and data

To demonstrate the various functionalities of the function, we will
create a small dataset. We have three hypotetical lab-values: LDH, ANC
and Lymphocytes and two categorical variables, grade and stage. Each
lab-value has been measured at 10 time points (visits), which can
further be divided into three visit groups (baseline, treatment and
follow-up). In addition, we have a grouping variable called ‘arm’.

``` r

data<-NULL
visit <- c(paste0(rep("Visit ", 10), rbind(c(1:10))),
           paste0(rep("Visit ", 10), rbind(c(1:10))),
           paste0(rep("Visit ", 10), rbind(c(1:10))))
data <- as.data.frame(cbind( visit, rnorm(30)))
data<-as.data.frame(rbind(data, data, data, data, data))
data$visitgroup<- ifelse(data$visit %in% c("Visit 1", "Visit 2"), "Baseline", ifelse(data$visit %in% c("Visit 3", "Visit 4"), "Treatment", "Follow-up"))
data$visitgroup<-factor(data$visitgroup, levels = c("Baseline", "Treatment", "Follow-up"))
data$LDH<-rnorm(150)
data$Lymphocytes<-rnorm(150)
data$ANC<-rnorm(150)
data$LDH[3]<-NA
data$stage <- c(rep("II", 75), rep("I", 75))
data$stage <- ifelse(data$Lymphocytes>1.5, NA, data$stage)
data$grade <- c(rep("IV", 50), rep("III", 50), rep("II", 25), rep("I", 25))
data$arm<- c(rep("Arm A", 70), rep("Arm B", 80))
```

### Basic table

Now, we use
[`summarySCI::summaryLevels`](https://sakk-statistics.github.io/summarySCI/reference/summaryLevels.md)
to create a summary table for median and range of each lab-value by
visit.

``` r

summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "stage"),
         visit = "visit", 
         add_n = TRUE)
```

[TABLE]

### Add visit group

We can the visit group to group visits accordingly. Visitgroup needs to
be an ordered factor.

``` r

summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "stage"),
         visitgroup = "visitgroup",
         visit = "visit")
```

[TABLE]

### By group

We can stratify the table by groups via the `group` argument. The
overall column can still be shown if desired, using the `overall = TRUE`
argument. A maximum of 3 groups are supported.

``` r

summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "stage"),
         group = "arm",
         visitgroup = "visitgroup",
         visit = "visit")
```

[TABLE]

``` r

summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "stage"),
         group = "arm",
         overall = TRUE,
         visitgroup = "visitgroup",
         visit = "visit")
```

[TABLE]

### Add N

Sample size can be shown for each column, if the option `add_n` is set
to `TRUE`.

``` r

summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "stage"),
         group = "arm",
         overall = TRUE,
         visitgroup = "visitgroup",
         visit = "visit",
         add_n = TRUE)
```

[TABLE]

### Change labels

The labels can be changed by using the “labels” statement.

``` r

summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "stage"),
         labels=c("LDH (g/L)", "Lymphocytes (g)", "Stage"),
         group = "arm",
         overall = TRUE,
         visitgroup = "visitgroup",
         visit = "visit",
         add_n = TRUE)
```

[TABLE]

## Further steps:

- improve costumization
