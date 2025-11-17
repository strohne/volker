# Output test statistics and effect size from a logistic regression of one metric predictor

**Not yet implemented. The future will come.**

## Usage

``` r
effect_counts_one_cor(data, col, cross, clean = TRUE, labels = TRUE, ...)
```

## Arguments

- data:

  A tibble.

- col:

  The column holding factor values.

- cross:

  The column holding metric values.

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [effect_counts](https://strohne.github.io/volker/reference/effect_counts.md).

## Value

A volker tibble.
