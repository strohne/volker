# Check whether a parameter value is from a valid set

Check whether a parameter value is from a valid set

## Usage

``` r
check_is_param(
  value,
  allowed,
  allownull = FALSE,
  allowmultiple = FALSE,
  expand = FALSE,
  stopit = TRUE,
  msg = NULL
)
```

## Arguments

- value:

  A character value.

- allowed:

  Allowed values.

- allownull:

  Whether to allow NULL values.

- allowmultiple:

  Whether to allow multiple values.

- stopit:

  Whether to stop execution if the value is invalid.

- msg:

  A custom error message if the check fails.

## Value

logical whether method is valid.
