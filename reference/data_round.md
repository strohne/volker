# Round and format selected numeric columns

Round and format specified numeric columns in a data frame to a fixed
number of decimal places.

## Usage

``` r
data_round(data, cols, digits)
```

## Arguments

- data:

  A data frame or tibble.

- cols:

  A tidyselect expression specifying which columns to round (e.g.,
  `c(var1, var2)` or `starts_with("score")`).

- digits:

  Integer; number of decimal places to round.

## Value

The input data frame, with the specified numeric columns rounded and
formatted as character vectors.

## Details

For each selected numeric column:

- The column is rounded using
  [`round()`](https://rdrr.io/r/base/Round.html).

- It is then formatted as a character vector with a fixed number of
  decimal places using
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

- Missing values (`NA`) are preserved as `NA_character_`.

- Original non-essential attributes (except `class` and `levels`) are
  restored.
