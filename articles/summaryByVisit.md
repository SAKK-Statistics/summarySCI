# summaryByVisit

The function
[`summaryByVisit()`](https://sakk-statistics.github.io/summarySAKK/reference/summaryByVisit.md)
produces a table with descriptive statistics for continues variables at
different time points (visits). It is largely based on the function
[`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
and
[`gtsummary::tbl_strata_nested_stack`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_strata_nested_stack.html).
The changes as compared to these functions are:

- A hierarchical summary table is created by variable (lab-value), visit
  and visitgroup.

### Setup and data

To demonstrate the various functionalities of the function, we will
create a small dataset. We have three hypotetical lab-values: LDH, ANC
and Lymphocytes. Each lab-value has been measured at 10 time points
(visits), which can further be divided into three visit groups
(baseline, treatment and follow-up). In addition, we have a grouping
variable called ‘arm’.

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
data$arm<- c(rep("Arm A", 70), rep("Arm B", 80))
```

### Basic table

Now, we use
[`summarySCI::summaryLevels`](https://sakk-statistics.github.io/summarySAKK/reference/summaryLevels.md)
to create a summary table for median and range of each lab-value by
visit.

``` r
summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "ANC"),
         visit = "visit", 
         add_n = TRUE)
```

| Visit              | N   | N = 151          |
|--------------------|-----|------------------|
| LDH                |     |                  |
| Visit 1            | 15  | 0.1 (-1.9, 2.0)  |
| Visit 2            | 15  | 0.1 (-1.4, 1.3)  |
| Visit 3            | 14  | 0.5 (-1.1, 1.9)  |
| Visit 4            | 15  | 0.0 (-1.3, 1.6)  |
| Visit 5            | 15  | 0.1 (-1.8, 1.3)  |
| Visit 6            | 15  | 0.5 (-1.0, 2.8)  |
| Visit 7            | 15  | -0.5 (-2.6, 1.1) |
| Visit 8            | 15  | -0.2 (-1.7, 2.7) |
| Visit 9            | 15  | 0.2 (-1.5, 1.3)  |
| Visit 10           | 15  | 0.2 (-1.9, 2.2)  |
| Lymphocytes        |     |                  |
| Visit 1            | 15  | -0.3 (-1.2, 1.1) |
| Visit 2            | 15  | -0.5 (-2.2, 1.4) |
| Visit 3            | 15  | 0.0 (-1.7, 1.5)  |
| Visit 4            | 15  | 0.1 (-1.8, 1.5)  |
| Visit 5            | 15  | 0.4 (-2.4, 2.1)  |
| Visit 6            | 15  | 0.1 (-2.1, 2.0)  |
| Visit 7            | 15  | 0.3 (-1.4, 2.2)  |
| Visit 8            | 15  | 0.4 (-1.0, 2.6)  |
| Visit 9            | 15  | 0.6 (-2.1, 1.9)  |
| Visit 10           | 15  | 0.2 (-1.7, 2.6)  |
| ANC                |     |                  |
| Visit 1            | 15  | 0.2 (-1.4, 1.8)  |
| Visit 2            | 15  | -0.3 (-2.0, 2.1) |
| Visit 3            | 15  | -0.1 (-1.2, 1.8) |
| Visit 4            | 15  | -0.5 (-2.2, 1.8) |
| Visit 5            | 15  | -0.1 (-2.9, 1.4) |
| Visit 6            | 15  | 0.6 (-2.1, 2.0)  |
| Visit 7            | 15  | -0.6 (-1.7, 0.6) |
| Visit 8            | 15  | -0.4 (-1.9, 2.1) |
| Visit 9            | 15  | 0.1 (-0.7, 1.5)  |
| Visit 10           | 15  | 0.2 (-1.8, 1.2)  |
| 1Median (Min, Max) |     |                  |

### Add visit group

We can the visit group to group visits accordingly. Visitgroup needs to
be an ordered factor.

``` r
summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "ANC"),
         visitgroup = "visitgroup",
         visit = "visit")
```

| Visit              | N = 151          |
|--------------------|------------------|
| LDH                |                  |
| Baseline           |                  |
| Visit 1            | 0.1 (-1.9, 2.0)  |
| Visit 2            | 0.1 (-1.4, 1.3)  |
| Treatment          |                  |
| Visit 3            | 0.5 (-1.1, 1.9)  |
| Visit 4            | 0.0 (-1.3, 1.6)  |
| Follow-up          |                  |
| Visit 5            | 0.1 (-1.8, 1.3)  |
| Visit 6            | 0.5 (-1.0, 2.8)  |
| Visit 7            | -0.5 (-2.6, 1.1) |
| Visit 8            | -0.2 (-1.7, 2.7) |
| Visit 9            | 0.2 (-1.5, 1.3)  |
| Visit 10           | 0.2 (-1.9, 2.2)  |
| Lymphocytes        |                  |
| Baseline           |                  |
| Visit 1            | -0.3 (-1.2, 1.1) |
| Visit 2            | -0.5 (-2.2, 1.4) |
| Treatment          |                  |
| Visit 3            | 0.0 (-1.7, 1.5)  |
| Visit 4            | 0.1 (-1.8, 1.5)  |
| Follow-up          |                  |
| Visit 5            | 0.4 (-2.4, 2.1)  |
| Visit 6            | 0.1 (-2.1, 2.0)  |
| Visit 7            | 0.3 (-1.4, 2.2)  |
| Visit 8            | 0.4 (-1.0, 2.6)  |
| Visit 9            | 0.6 (-2.1, 1.9)  |
| Visit 10           | 0.2 (-1.7, 2.6)  |
| ANC                |                  |
| Baseline           |                  |
| Visit 1            | 0.2 (-1.4, 1.8)  |
| Visit 2            | -0.3 (-2.0, 2.1) |
| Treatment          |                  |
| Visit 3            | -0.1 (-1.2, 1.8) |
| Visit 4            | -0.5 (-2.2, 1.8) |
| Follow-up          |                  |
| Visit 5            | -0.1 (-2.9, 1.4) |
| Visit 6            | 0.6 (-2.1, 2.0)  |
| Visit 7            | -0.6 (-1.7, 0.6) |
| Visit 8            | -0.4 (-1.9, 2.1) |
| Visit 9            | 0.1 (-0.7, 1.5)  |
| Visit 10           | 0.2 (-1.8, 1.2)  |
| 1Median (Min, Max) |                  |

### By group

We can stratify the table by groups via the `group` argument. The
overall column can still be shown if desired, using the `overall = TRUE`
argument. A maximum of 3 groups are supported.

``` r
summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "ANC"),
         group = "arm",
         visitgroup = "visitgroup",
         visit = "visit")
```

[TABLE]

``` r
summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "ANC"),
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
         vars = c("LDH", "Lymphocytes", "ANC"),
         group = "arm",
         overall = TRUE,
         visitgroup = "visitgroup",
         visit = "visit",
         add_n = TRUE)
```

[TABLE]

## Further steps:

- improve costumization
