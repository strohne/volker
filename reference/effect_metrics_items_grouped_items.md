# Compare groups for each item with multiple target items by calculating F-statistics and effect sizes

**Not yet implemented. The future will come.**

## Usage

``` r
effect_metrics_items_grouped_items(data, cols, cross, clean = TRUE, ...)
```

## Arguments

- data:

  A tibble containing item measures.

- cols:

  Tidyselect item variables (e.g. starts_with...).

- cross:

  The grouping items.

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [effect_counts](https://strohne.github.io/volker/reference/effect_counts.md).

## Value

A volker tibble.
