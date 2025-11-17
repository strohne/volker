# Knit volker tables

Numbers are rounded by three mechanisms:

- Each column may have an attribute "round" with the numbers of digits.
  This results in a character vector with a fixed number of digits. NA
  values are replaced by an empty string.

- The data frame may have a column ".digits" with a number of digits.
  All numeric values are rounded to the number of digits in this column,
  the column is discarded afterwards.

- The data frame may have an attribute "digits" resulting in all numbers
  in the data frame being rounded to that number. The constant
  VLKR_NORMAL_DIGITS is used as a fallback.

## Usage

``` r
.knit_table(df, ...)
```

## Arguments

- df:

  Data frame.

## Value

Formatted table produced by
[kable](https://rdrr.io/pkg/knitr/man/kable.html).
