# Correlate the values in multiple items with multiple metric columns and output effect sizes and tests

**Not yet implemented. The future will come.**

## Usage

``` r
effect_counts_items_cor_items(data, cols, cross, clean = TRUE, ...)
```

## Arguments

- data:

  A tibble containing item measures.

- cols:

  Tidyselect item variables (e.g. starts_with...).

- cross:

  The metric target columns.

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [effect_counts](https://strohne.github.io/volker/reference/effect_counts.md).

## Value

A volker tibble.
