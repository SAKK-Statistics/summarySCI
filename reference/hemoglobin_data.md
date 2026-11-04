# Hemoglobin data (to illustrate `boxplotSCI()` and `summaryByVisit()`)

A dataset containing the hemoglobin value of 169 patients over 9 visits.
Adapted from SAKK 08/14.

## Usage

``` r
data(hemoglobin_data)
```

## Format

### `hemoglobin_data`

A data frame with 1307 rows and 5 columns:

- ID:

  Patient ID

- Arm:

  Experimental arm ("Arm A", "Arm B")

- visit:

  Visit (character, "Cycle 1" to "Cycle 9")

- visit_nr:

  Visit number (numeric, 1 to 9)

- hb:

  Hemoglobin value (g/L)

## Source

SAKK
