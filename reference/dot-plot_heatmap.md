# Helper function: Heatmap

Helper function: Heatmap

## Usage

``` r
.plot_heatmap(
  data,
  values_col,
  numbers_col = NULL,
  base = NULL,
  title = TRUE,
  labels = TRUE
)
```

## Arguments

- data:

  A tibble with item combinations in the first two columns. Only if the
  item values are equal in both columns, titles are added to the axes.

- values_col:

  Name of the column containing correlation values, a character value.

- numbers_col:

  Name of the column containing values to plot on the tiles or NULL to
  hide numbers.

- base:

  Character value; the baseline note, including the number of cases.

- title:

  If TRUE (default) shows a plot title derived from the column labels.
  Disable the title with FALSE or provide a custom title as character
  value.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

## Value

A ggplot object
