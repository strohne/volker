# Split a metric column into categories based on the median

Split a metric column into categories based on the median

## Usage

``` r
.tab_split(data, col, labels = TRUE)
```

## Arguments

- data:

  A data frame containing the column to be split.

- col:

  The column to split.

- labels:

  Logical; if `TRUE` (default), use custom labels for the split
  categories based on the column title. If `FALSE`, use the column name
  directly.

## Value

A data frame with the specified column converted into categorical labels
based on its median value. The split threshold (median) is stored as an
attribute of the column.
