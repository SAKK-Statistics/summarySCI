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
[`summarySCI::summaryByVisit`](https://sakk-statistics.github.io/summarySCI/reference/summaryByVisit.md)
to create a summary table for median and range of each lab-value by
visit.

``` r

summaryByVisit(data = data,
         vars = c("LDH", "Lymphocytes", "stage"),
         visit = "visit", 
         add_n = TRUE)
```

| Visit                     | N1  | N = 152          |
|---------------------------|-----|------------------|
| LDH                       |     |                  |
| Visit 1                   | 15  | 0.1 (-1.9, 2.0)  |
| Visit 2                   | 15  | 0.1 (-1.4, 1.3)  |
| Visit 3                   | 14  | 0.5 (-1.1, 1.9)  |
| Visit 4                   | 15  | 0.0 (-1.3, 1.6)  |
| Visit 5                   | 15  | 0.1 (-1.8, 1.3)  |
| Visit 6                   | 15  | 0.5 (-1.0, 2.8)  |
| Visit 7                   | 15  | -0.5 (-2.6, 1.1) |
| Visit 8                   | 15  | -0.2 (-1.7, 2.7) |
| Visit 9                   | 15  | 0.2 (-1.5, 1.3)  |
| Visit 10                  | 15  | 0.2 (-1.9, 2.2)  |
| Lymphocytes               |     |                  |
| Visit 1                   | 15  | -0.3 (-1.2, 1.1) |
| Visit 2                   | 15  | -0.5 (-2.2, 1.4) |
| Visit 3                   | 15  | 0.0 (-1.7, 1.5)  |
| Visit 4                   | 15  | 0.1 (-1.8, 1.5)  |
| Visit 5                   | 15  | 0.4 (-2.4, 2.1)  |
| Visit 6                   | 15  | 0.1 (-2.1, 2.0)  |
| Visit 7                   | 15  | 0.3 (-1.4, 2.2)  |
| Visit 8                   | 15  | 0.4 (-1.0, 2.6)  |
| Visit 9                   | 15  | 0.6 (-2.1, 1.9)  |
| Visit 10                  | 15  | 0.2 (-1.7, 2.6)  |
| stage                     |     |                  |
| Visit 1                   | 15  |                  |
| I                         |     | 7.0 (46.7%)      |
| II                        |     | 8.0 (53.3%)      |
| Visit 2                   | 15  |                  |
| I                         |     | 7.0 (46.7%)      |
| II                        |     | 8.0 (53.3%)      |
| Visit 3                   | 15  |                  |
| I                         |     | 7.0 (46.7%)      |
| II                        |     | 8.0 (53.3%)      |
| Visit 4                   | 14  |                  |
| I                         |     | 7.0 (46.7%)      |
| II                        |     | 7.0 (46.7%)      |
| Missing                   |     | 1.0 (6.7%)       |
| Visit 5                   | 12  |                  |
| I                         |     | 5.0 (33.3%)      |
| II                        |     | 7.0 (46.7%)      |
| Missing                   |     | 3.0 (20.0%)      |
| Visit 6                   | 13  |                  |
| I                         |     | 7.0 (46.7%)      |
| II                        |     | 6.0 (40.0%)      |
| Missing                   |     | 2.0 (13.3%)      |
| Visit 7                   | 13  |                  |
| I                         |     | 6.0 (40.0%)      |
| II                        |     | 7.0 (46.7%)      |
| Missing                   |     | 2.0 (13.3%)      |
| Visit 8                   | 13  |                  |
| I                         |     | 7.0 (46.7%)      |
| II                        |     | 6.0 (40.0%)      |
| Missing                   |     | 2.0 (13.3%)      |
| Visit 9                   | 14  |                  |
| I                         |     | 7.0 (46.7%)      |
| II                        |     | 7.0 (46.7%)      |
| Missing                   |     | 1.0 (6.7%)       |
| Visit 10                  | 13  |                  |
| I                         |     | 7.0 (46.7%)      |
| II                        |     | 6.0 (40.0%)      |
| Missing                   |     | 2.0 (13.3%)      |
| 1N without missing values |     |                  |
| 2Median (Min, Max); n (%) |     |                  |

### Add visit group

We can the visit group to group visits accordingly. Visitgroup needs to
be an ordered factor.

``` r

summaryByVisit(data,
         vars = c("LDH", "Lymphocytes", "stage"),
         visitgroup = "visitgroup",
         visit = "visit")
```

| Visit                     | N = 151          |
|---------------------------|------------------|
| LDH                       |                  |
| Baseline                  |                  |
| Visit 1                   | 0.1 (-1.9, 2.0)  |
| Visit 2                   | 0.1 (-1.4, 1.3)  |
| Treatment                 |                  |
| Visit 3                   | 0.5 (-1.1, 1.9)  |
| Visit 4                   | 0.0 (-1.3, 1.6)  |
| Follow-up                 |                  |
| Visit 5                   | 0.1 (-1.8, 1.3)  |
| Visit 6                   | 0.5 (-1.0, 2.8)  |
| Visit 7                   | -0.5 (-2.6, 1.1) |
| Visit 8                   | -0.2 (-1.7, 2.7) |
| Visit 9                   | 0.2 (-1.5, 1.3)  |
| Visit 10                  | 0.2 (-1.9, 2.2)  |
| Lymphocytes               |                  |
| Baseline                  |                  |
| Visit 1                   | -0.3 (-1.2, 1.1) |
| Visit 2                   | -0.5 (-2.2, 1.4) |
| Treatment                 |                  |
| Visit 3                   | 0.0 (-1.7, 1.5)  |
| Visit 4                   | 0.1 (-1.8, 1.5)  |
| Follow-up                 |                  |
| Visit 5                   | 0.4 (-2.4, 2.1)  |
| Visit 6                   | 0.1 (-2.1, 2.0)  |
| Visit 7                   | 0.3 (-1.4, 2.2)  |
| Visit 8                   | 0.4 (-1.0, 2.6)  |
| Visit 9                   | 0.6 (-2.1, 1.9)  |
| Visit 10                  | 0.2 (-1.7, 2.6)  |
| stage                     |                  |
| Baseline                  |                  |
| Visit 1                   |                  |
| I                         | 7.0 (46.7%)      |
| II                        | 8.0 (53.3%)      |
| Visit 2                   |                  |
| I                         | 7.0 (46.7%)      |
| II                        | 8.0 (53.3%)      |
| Treatment                 |                  |
| Visit 3                   |                  |
| I                         | 7.0 (46.7%)      |
| II                        | 8.0 (53.3%)      |
| Visit 4                   |                  |
| I                         | 7.0 (46.7%)      |
| II                        | 7.0 (46.7%)      |
| Missing                   | 1.0 (6.7%)       |
| Follow-up                 |                  |
| Visit 5                   |                  |
| I                         | 5.0 (33.3%)      |
| II                        | 7.0 (46.7%)      |
| Missing                   | 3.0 (20.0%)      |
| Visit 6                   |                  |
| I                         | 7.0 (46.7%)      |
| II                        | 6.0 (40.0%)      |
| Missing                   | 2.0 (13.3%)      |
| Visit 7                   |                  |
| I                         | 6.0 (40.0%)      |
| II                        | 7.0 (46.7%)      |
| Missing                   | 2.0 (13.3%)      |
| Visit 8                   |                  |
| I                         | 7.0 (46.7%)      |
| II                        | 6.0 (40.0%)      |
| Missing                   | 2.0 (13.3%)      |
| Visit 9                   |                  |
| I                         | 7.0 (46.7%)      |
| II                        | 7.0 (46.7%)      |
| Missing                   | 1.0 (6.7%)       |
| Visit 10                  |                  |
| I                         | 7.0 (46.7%)      |
| II                        | 6.0 (40.0%)      |
| Missing                   | 2.0 (13.3%)      |
| 1Median (Min, Max); n (%) |                  |

### By group

We can stratify the table by groups via the `group` argument. The
overall column can still be shown if desired, using the `overall = TRUE`
argument. A maximum of 3 groups are supported.

``` r

summaryByVisit(data = data,
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
         labels=list(LDH="LDH (g/L)", Lymphocytes="Lymphocytes (g)", stage="Stage"),
         group = "arm",
         overall = TRUE,
         visitgroup = "visitgroup",
         visit = "visit",
         add_n = TRUE)
```

[TABLE]

## Further steps:

- improve costumization
