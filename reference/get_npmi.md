# Calculate nmpi

Calculate nmpi

## Usage

``` r
get_npmi(
  data,
  col,
  cross,
  category = NULL,
  smoothing = 0,
  adjust = "fdr",
  test = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- col:

  The column holding factor values.

- cross:

  The column to correlate.

- category:

  A vector of values to focus. If not null, all other values will be
  removed from the result.

- smoothing:

  Add pseudocount. Calculate the pseudocount based on the number of
  trials to apply Laplace's rule of succession.

- adjust:

  Performing multiple significance tests inflates the alpha error. Thus,
  p values need to be adjusted according to the number of tests. Set a
  method supported by
  `stats::`[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html), e.g.
  "fdr" (the default) or "bonferroni". Disable adjustment with FALSE.

- test:

  Boolean; whether to perform significance tests (default TRUE).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [tab_counts](https://strohne.github.io/volker/reference/tab_counts.md).

## Value

A volker tibble.
