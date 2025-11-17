# Test whether the correlation is different from zero

The correlation is calculated using
`stats::`[`cor.test`](https://rdrr.io/r/stats/cor.test.html).

## Usage

``` r
effect_metrics_one_cor(
  data,
  col,
  cross,
  method = "pearson",
  adjust = "fdr",
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- col:

  The column holding metric values.

- cross:

  The column holding metric values to correlate.

- method:

  The output metrics, TRUE or pearson = Pearson's R, spearman =
  Spearman's rho.

- adjust:

  Performing multiple significance tests inflates the alpha error. Thus,
  p values need to be adjusted according to the number of tests. Set a
  method supported by
  `stats::`[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html), e.g.
  "fdr" (the default) or "bonferroni". Disable adjustment with FALSE.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [effect_metrics](https://strohne.github.io/volker/reference/effect_metrics.md).

## Value

A volker table containing the requested statistics.

If `method = "pearson"`:

- **R-squared**: Coefficient of determination.

- **n**: Number of cases the calculation is based on.

- **Pearson's r**: Correlation coefficient.

- **ci low / ci high**: Lower and upper bounds of the 95% confidence
  interval.

- **df**: Degrees of freedom.

- **t**: t-statistic.

- **p**: p-value for the statistical test, indicating whether the
  correlation differs from zero.

- **stars**: Significance stars based on the p-value (\*, \*\*, \*\*\*).

If `method = "spearman"`:

- **Spearman's rho** is displayed instead of Pearson's r.

- **S-statistic** is used instead of the t-statistic.

## Examples

``` r
library(volker)
data <- volker::chatgpt

effect_metrics_one_cor(data, sd_age, use_private, metric = TRUE)
#> 
#> 
#> |Statistic   | value|
#> |:-----------|-----:|
#> |Pearson's r | -0.19|
#> |R-squared   |  0.03|
#> |n           |   101|
#> |ci low      | -0.37|
#> |ci high     |  0.01|
#> |df          |    99|
#> |t           | -1.88|
#> |p           | 0.063|
#> |stars       |     .|
```
