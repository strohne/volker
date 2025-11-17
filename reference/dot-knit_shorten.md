# Compact table printing with shortened names and values

Truncates long column names and long character values, for more readable
console output.

## Usage

``` r
.knit_shorten(df)
```

## Arguments

- df:

  A data frame or tibble.

## Value

A data fram with shortened column names and cell content.

## Details

The default column name length is 30 and the cell values length is 40.
Override with `options(vlkr.trunc.columns=20)` and
`options(vlkr.trunc.cells=20)`.
