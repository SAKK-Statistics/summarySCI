# Natural ordering of levels for character variables

Values starting with a number are sorted numerically by that number
(e.g. "3" \< "10", "2 doses" \< "10 doses") and come first. A leading
comparison operator is taken into account: "\<10%" comes just before
"10%", "\>90%" just after "90%" (also "\<=", "\>=", and the symbols for
these). All other values follow alphabetically, with embedded numbers
compared numerically ("Arm 2" \< "Arm 10"). NA is not a level.

## Usage

``` r
natural_levels(x)
```

## Arguments

- x:

  A character vector.

## Value

Character vector of levels in natural order.
