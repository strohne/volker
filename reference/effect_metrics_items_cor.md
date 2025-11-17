# Output correlation coefficients for items and one metric variable

The correlation is calculated using
`stats::`[`cor.test`](https://rdrr.io/r/stats/cor.test.html).

## Usage

``` r
effect_metrics_items_cor(
  data,
  cols,
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

  A tibble containing item measures.

- cols:

  Tidyselect item variables (e.g. starts_with...).

- cross:

  The column holding metric values to correlate.

- method:

  The output metrics, pearson = Pearson's R, spearman = Spearman's rho.

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

A volker table containing itemwise correlations:

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

effect_metrics_items_cor(
  data, starts_with("cg_adoption_adv"), sd_age
)
#> 
#> 
#> |Expectations: Correlation w...           | Pearson's r| R-squared|  n| ci low| ci high| df|     t|     p| stars|
#> |:----------------------------------------|-----------:|---------:|--:|------:|-------:|--:|-----:|-----:|-----:|
#> |ChatGPT has clear advantages compared... |       -0.12|      0.01| 99|  -0.31|    0.08| 97| -1.16| 0.475|      |
#> |Using ChatGPT brings financial benefits. |       -0.09|      0.01| 99|  -0.29|    0.11| 97| -0.93| 0.475|      |
#> |Using ChatGPT is advantageous in many... |       -0.06|      0.00| 99|  -0.25|    0.14| 97| -0.56| 0.579|      |
#> |Compared to other systems, using Chat... |       -0.12|      0.01| 99|  -0.31|    0.08| 97| -1.18| 0.475|      |
#> 
#> n=99. 2 missing case(s) omitted. Adjusted significance p values with fdr method.
#> 
```
