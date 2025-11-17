# Adjust p-values from multiple tests and optionally annotate significance stars

Adjust p-values from multiple tests and optionally annotate significance
stars

## Usage

``` r
adjust_p(df, col, method = "fdr", digits = 3, stars = TRUE)
```

## Arguments

- df:

  A data frame or tibble containing the column to adjust.

- col:

  A tidyselect expression specifying the p-value column to adjust.

- method:

  Character string specifying the p-value adjustment method. See
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) for available
  methods. Disable adjustment with FALSE.

- digits:

  Integer; number of decimal places for rounding.

- stars:

  Logical or character; if `TRUE`, add a "stars" column with
  significance symbols (e.g., `"***"`, `"**"`, `"*"`) based on the
  adjusted p-values. If set to a character value it determines the new
  column name.

## Value

A modified data frame with:

- Adjusted p-values in the selected column.

- (Optionally) a new column `stars` containing significance symbols.

- An attribute `"adjust"` on the data frame storing the method.

- A `"round"` attribute on the p-value column.
