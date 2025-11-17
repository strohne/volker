# Detect whether a scale is a numeric sequence

From all values in the selected columns, the numbers are extracted. If
no numeric values can be found, returns 0. Otherwise, if any positive
values form an ascending sequence, returns -1. In all other cases,
returns 1.

## Usage

``` r
get_direction(data, cols, extract = TRUE)
```

## Arguments

- data:

  The dataframe.

- cols:

  The tidy selection.

- extract:

  Whether to extract numeric values from characters.

## Value

0 = an undirected scale, -1 = descending values, 1 = ascending values.
