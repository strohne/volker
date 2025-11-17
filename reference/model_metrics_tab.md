# Output a regression table with estimates and macro statistics for multiple categorical or metric independent variables

The regression output comes from
`stats::`[`lm`](https://rdrr.io/r/stats/lm.html). The effect sizes are
calculated by
`heplots::`[`etasq`](https://friendly.github.io/heplots/reference/etasq.html).
The variance inflation is calculated by
`car::`[`vif`](https://rdrr.io/pkg/car/man/vif.html). The standardized
beta (in the column standard beta) is calculated by multiplying the
estimate with the ratio `x_sd / y_sd` where x_sd contains the standard
deviation of the predictor values and y_sd the standard deviation of the
predicted value.

**\[experimental\]**

## Usage

``` r
model_metrics_tab(
  data,
  col,
  categorical,
  metric,
  interactions = NULL,
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

  The target column holding metric values.

- categorical:

  A tidy column selection holding independet categorical variables.

- metric:

  A tidy column selection holding independent metric variables.

- interactions:

  A vector of interaction effects to calculate. Each interaction effect
  should be provided as multiplication of the variables. Example:
  `c(sd_gender * adopter)`.

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

A volker list object containing volker tables with the requested
statistics.

## Examples

``` r
library(volker)
data <- volker::chatgpt

data |>
  filter(sd_gender != "diverse") |>
  model_metrics_tab(use_work, categorical = c(sd_gender, adopter), metric = sd_age)
#> 
#> 
#> |Term                                     | estimate| ci low| ci high| standard beta| standard error|     t|     p| stars|
#> |:----------------------------------------|--------:|------:|-------:|-------------:|--------------:|-----:|-----:|-----:|
#> |(Intercept)                              |     4.67|   3.64|    5.70|              |           0.52|  9.00| 0.000|   ***|
#> |female (Reference)                       |         |       |        |              |               |      |      |      |
#> |male                                     |    -0.48|  -1.02|    0.06|         -0.17|           0.27| -1.75| 0.124|      |
#> |I try new offers immediately (Reference) |         |       |        |              |               |      |      |      |
#> |I try new offers rather quickly          |    -1.52|  -2.29|   -0.76|         -0.53|           0.38| -3.96| 0.000|   ***|
#> |I wait until offers establish themselves |    -1.96|  -2.86|   -1.06|         -0.59|           0.45| -4.34| 0.000|   ***|
#> |I only use new offers when I have no ... |    -1.36|  -3.99|    1.27|         -0.10|           1.33| -1.03| 0.307|      |
#> |sd_age                                   |    -0.01|  -0.03|    0.00|         -0.15|           0.01| -1.58| 0.142|      |
#> 
#> Adjusted significance p values with fdr method.
#> 
#> 
#> 
#> |Item      | Partial Eta Squared| Sum of Squares| Df|    F|     p| stars| GVIF| GVIF^(1/(2*Df))|
#> |:---------|-------------------:|--------------:|--:|----:|-----:|-----:|----:|---------------:|
#> |sd_gender |                0.03|           4.96|  1| 3.08| 0.118|      | 1.10|            1.05|
#> |adopter   |                0.18|          32.78|  3| 6.78| 0.001|    **| 1.12|            1.02|
#> |sd_age    |                0.03|           4.01|  1| 2.49| 0.118|      | 1.05|            1.02|
#> |Residuals |                    |         151.43| 94|     |      |      |     |                |
#> 
#> Adjusted significance p values with fdr method.
#> 
#> 
#> 
#> |Statistic                     |  value|
#> |:-----------------------------|------:|
#> |Adjusted R squared            |   0.17|
#> |Degrees of freedom            |      5|
#> |Residuals' degrees of freedom |     94|
#> |AIC                           | 339.28|
#> |F                             |   5.01|
#> |p                             |      0|
#> |stars                         |    ***|
```
