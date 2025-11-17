# Output correlation coefficients for multiple items

The correlation is calculated using
`stats::`[`cor.test`](https://rdrr.io/r/stats/cor.test.html).

## Usage

``` r
effect_metrics_items_cor_items(
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

  Tidyselect item variables (e.g. starts_with...).

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

A volker table containing correlations.

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

effect_metrics_items_cor_items(
  data,
  starts_with("cg_adoption_adv"),
  starts_with("use"),
  metric = TRUE
)
#> 
#> 
#> |Expectations                             |                   Usage| Pearson's r| R-squared|  n| ci low| ci high| df|    t|     p| stars|
#> |:----------------------------------------|-----------------------:|-----------:|---------:|--:|------:|-------:|--:|----:|-----:|-----:|
#> |ChatGPT has clear advantages compared... |      in private context|        0.50|      0.25| 99|   0.33|    0.63| 97| 5.61| 0.000|   ***|
#> |ChatGPT has clear advantages compared... | in professional context|        0.27|      0.07| 99|   0.07|    0.44| 97| 2.73| 0.009|    **|
#> |Using ChatGPT brings financial benefits. |      in private context|        0.17|      0.03| 99|  -0.03|    0.36| 97| 1.73| 0.087|     .|
#> |Using ChatGPT brings financial benefits. | in professional context|        0.53|      0.28| 99|   0.37|    0.66| 97| 6.19| 0.000|   ***|
#> |Using ChatGPT is advantageous in many... |      in private context|        0.34|      0.12| 99|   0.16|    0.51| 97| 3.59| 0.001|   ***|
#> |Using ChatGPT is advantageous in many... | in professional context|        0.35|      0.12| 99|   0.17|    0.51| 97| 3.71| 0.001|   ***|
#> |Compared to other systems, using Chat... |      in private context|        0.47|      0.22| 99|   0.30|    0.61| 97| 5.28| 0.000|   ***|
#> |Compared to other systems, using Chat... | in professional context|        0.27|      0.07| 99|   0.07|    0.44| 97| 2.73| 0.009|    **|
#> 
#> n=99. 2 missing case(s) omitted. Adjusted significance p values with fdr method.
#> 
```
