# Calculate a metric by groups

Calculate a metric by groups

## Usage

``` r
skim_grouped(data, cols, cross, value = "mean", labels = TRUE)
```

## Arguments

- data:

  A tibble.

- cols:

  The item columns that hold the values to summarize.

- cross:

  The column holding groups to compare.

- value:

  The metric to extract from the skim result, e.g. mean or sd.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

## Value

A tibble with each item in a row, a total column and columns for all
groups.
