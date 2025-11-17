# Check whether there is no overlap between two column sets

Check whether there is no overlap between two column sets

## Usage

``` r
check_nonequal_columns(data, cols, cross, msg = NULL)
```

## Arguments

- data:

  A data frame.

- cols:

  A tidyselection of columns.

- cross:

  A tidyselection of columns.

- msg:

  A custom error message if the check fails.

## Value

boolean Whether the two column sets are different.
