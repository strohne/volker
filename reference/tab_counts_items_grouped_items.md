# Correlation of categorical items with categorical items

Correlation of categorical items with categorical items

## Usage

``` r
tab_counts_items_grouped_items(
  data,
  cols,
  cross,
  method = "cramer",
  category = NULL,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble containing item measures.

- cols:

  Tidyselect item variables (e.g. starts_with...).

- cross:

  Tidyselect item variables (e.g. starts_with...).

- method:

  The method of correlation calculation:

  - `cramer` for Cramer's V,

  - `npmi` for Normalized Pointwise Mutual Information. Not implemented
    yet.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [plot_counts](https://strohne.github.io/volker/reference/plot_counts.md).

## Value

A volker tibble.
