# Calculate correlation and cooccurrence coefficients and test whether they are different from zero

This function is used to calculate coefficients for all pairwise items
by calling
[`get_correlation()`](https://strohne.github.io/volker/reference/get_correlation.md)
on each combination of the items in the `cols`- by `cross`-parameter.

## Usage

``` r
.effect_correlations(
  data,
  cols,
  cross,
  method = "pearson",
  category = NULL,
  test = TRUE,
  adjust = "fdr",
  labels = TRUE
)
```

## Arguments

- data:

  A tibble.

- cols:

  The columns holding metric values.

- cross:

  The columns holding metric values to correlate.

- method:

  The output metrics, pearson = Pearson's R, spearman = Spearman's rho,
  cramer = Cramer's V, npmi = Normalized Pointwise Mutual Information.
  The reported R square value is simply the square of Spearman's rho
  respective Pearson's r.

- category:

  Calculating NPMI for multiple items requires a focus category. By
  default, for logical column types, only TRUE values are counted. For
  other column types, the first category is counted. Accepts both
  character and numeric values to override default counting behavior.

- test:

  Boolean, whether to perform significance tests (default = TRUE).

- adjust:

  Performing multiple significance tests inflates the alpha error. Thus,
  p values need to be adjusted according to the number of tests. Set a
  method supported by
  `stats::`[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html), e.g.
  "fdr" (the default) or "bonferroni". Disable adjustment with FALSE.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

## Value

A tibble with correlation results.
