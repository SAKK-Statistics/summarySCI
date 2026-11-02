# Natural ordering of levels for character variables

Values starting with a number are sorted numerically by that number
(e.g. "3" \< "10", "2 doses" \< "10 doses") and come first; all other
values follow alphabetically, with embedded numbers compared numerically
("Arm 2" \< "Arm 10"). NA is not a level.

## Usage

``` r
natural_levels(x)
```

## Arguments

- x:

  A character vector.

## Value

Character vector of levels in natural order.
