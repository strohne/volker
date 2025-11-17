# Output test statistics and effect size for contingency tables

Chi squared is calculated using
`stats::`[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html). If any
cell contains less than 5 observations, the exact-parameter is set.

## Usage

``` r
effect_counts_one_grouped(data, col, cross, clean = TRUE, ...)
```

## Arguments

- data:

  A tibble.

- col:

  The column holding factor values.

- cross:

  The column holding groups to compare.

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [effect_counts](https://strohne.github.io/volker/reference/effect_counts.md).

## Value

A volker list with two volker tibbles. The first tibble contains npmi
values for each combinations:

- n Number the combination occurs.

- p_x Marginal share of the first category.

- p_y Marginal share of the second category.

- p_xy Share of the combination based on all combinations.

- ratio The ratio of p_xy to (p_x \* p_y).

- pmi Pointwise Mutual information, derived from the ratio.

- npmi Normalized Pointwise Mutual Information, derived from the pmi.

The second tibble contains effect sizes based on the cross table:

- Cramer's V: Effect size measuring the association between the two
  variables.

- n: Number of cases the calculation is based on.

- Chi-squared: Chi-Squared test statistic. If expected values are below
  5 in at least one cell, an exact Fisher test is conducted.

- df: Degrees of freedo of the chi-squared test. Empty for the exact
  Fisher test.

- p: p-value of the chi-squared test.

- stars: Significance stars based on p-value (\*, \*\*, \*\*\*).

## Details

Phi is derived from the Chi squared value by `sqrt(fit$statistic / n)`.
Cramer's V is derived by
`sqrt(phi / (min(dim(contingency)[1], dim(contingency)[2]) - 1))`.
Cramer's V is set to 1.0 for diagonal contingency matrices, indicating
perfect association.

## Examples

``` r
library(volker)
data <- volker::chatgpt

effect_counts_one_grouped(data, adopter, sd_gender)
#> 
#> 
#> |adopter                                  | sd_gender|  n|  p_x|  p_y| p_xy| ratio|   pmi|  npmi| fisher_p| fisher_stars|
#> |:----------------------------------------|---------:|--:|----:|----:|----:|-----:|-----:|-----:|--------:|------------:|
#> |I try new offers immediately             |    female|  2| 0.15| 0.40| 0.02|  0.34| -1.57| -0.28|    0.142|             |
#> |I try new offers immediately             |      male| 12| 0.15| 0.59| 0.12|  1.35|  0.43|  0.14|    0.187|             |
#> |I try new offers immediately             |   diverse|  1| 0.15| 0.01| 0.01|  6.73|  2.75|  0.41|    0.238|             |
#> |I try new offers rather quickly          |    female| 25| 0.62| 0.40| 0.25|  1.00|  0.00|  0.00|    1.000|             |
#> |I try new offers rather quickly          |      male| 38| 0.62| 0.59| 0.38|  1.02|  0.02|  0.02|    1.000|             |
#> |I wait until offers establish themselves |    female| 13| 0.22| 0.40| 0.13|  1.49|  0.58|  0.20|    0.142|             |
#> |I wait until offers establish themselves |      male|  9| 0.22| 0.59| 0.09|  0.69| -0.54| -0.15|    0.142|             |
#> |I only use new offers when I have no ... |      male|  1| 0.01| 0.59| 0.01|  1.68|  0.75|  0.11|    1.000|             |
#> 
#> Adjusted significance p values with fdr method.
#> 
#> 
#> 
#> |Statistic   | Value|
#> |:-----------|-----:|
#> |Cramer's V  |  0.26|
#> |Chi-squared | 13.48|
#> |n           |   101|
#> |df          |      |
#> |p           | 0.022|
#> |stars       |     *|
```
