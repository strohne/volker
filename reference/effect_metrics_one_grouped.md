# Output a regression table with estimates and macro statistics

The regression output comes from
`stats::`[`lm`](https://rdrr.io/r/stats/lm.html). T-test is performed
using `stats::`[`t.test`](https://rdrr.io/r/stats/t.test.html).
Normality check is performed using
`stats::`[`shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html).
Equality of variances across groups is assessed using
`car::`[`leveneTest`](https://rdrr.io/pkg/car/man/leveneTest.html).
Cohen's d is calculated using
`effectsize::`[`cohens_d`](https://easystats.github.io/effectsize/reference/cohens_d.html).

## Usage

``` r
effect_metrics_one_grouped(
  data,
  col,
  cross,
  method = "lm",
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

  The column holding groups to compare.

- method:

  A character vector of methods, e.g. c("t.test","lm"). Supported
  methods are t.test (only valid if the cross column contains two
  levels) and lm (regression results).

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

Regression table:

- **estimate**: Regression coefficient (unstandardized).

- **ci low / ci high**: lower and upper bound of the 95% confidence
  interval.

- **se**: Standard error of the estimate.

- **t**: t-statistic.

- **p**: p-value for the statistical test.

- **stars**: Significance stars based on p-value (\*, \*\*, \*\*\*).

Macro statistics:

- **Adjusted R-squared**: Adjusted coefficient of determination.

- **F**: F-statistic for the overall significance of the model.

- **df**: Degrees of freedom for the model.

- **residual df**: Residual degrees of freedom.

- **p**: p-value for the statistical test.

- **stars**: Significance stars based on p-value (\*, \*\*, \*\*\*).

If `method = t.test`:

### Shapiro-Wilk test (normality check):

- **W**: W-statistic from the Shapiro-Wilk normality test.

- **p**: p-value for the test.

- **normality**: Interpretation of the Shapiro-Wilk test.

### Levene test (equality of variances):

- **F**: F-statistic from the Levene test for equality of variances
  between groups.

- **p**: p-value for Levene's test.

- **variances**: Interpretation of the Levene test.

### Cohen's d (effect size):

- **d**: Standardized mean difference between the two groups.

- **ci low / ci high**: Lower and upper bounds of the 95% confidence
  interval.

### t-test

- **method**: Type of t-test performed (e.g., "Two Sample t-test").

- **difference**: Observed difference between group means.

- **ci low / ci high**: Lower and upper bounds of the 95% confidence
  interval.

- **se**: Estimated standard error of the difference.

- **df**: Degrees of freedom used in the t-test.

- **t**: t-statistic.

- **p**: p-value for the t-test.

- **stars**: Significance stars based on p-value (`*`, `**`, `***`).

## Examples

``` r
library(volker)
data <- volker::chatgpt

effect_metrics_one_grouped(data, sd_age, sd_gender)
#> 
#> 
#> |Term               | estimate| ci low| ci high|    se|     t|     p| stars|
#> |:------------------|--------:|------:|-------:|-----:|-----:|-----:|-----:|
#> |(Intercept)        |    37.52|  33.21|   41.84|  2.18| 17.24| 0.000|   ***|
#> |female (Reference) |         |       |        |      |      |      |      |
#> |male               |     3.69|  -1.88|    9.27|  2.81|  1.31| 0.288|      |
#> |diverse            |    -4.53| -32.18|   23.13| 13.94| -0.32| 0.746|      |
#> 
#> Adjusted significance p values with fdr method.
#> 
#> 
#> 
#> |Statistic          | Value|
#> |:------------------|-----:|
#> |Adjusted R-squared | -0.00|
#> |F                  |  0.98|
#> |df                 |     2|
#> |residual df        |    98|
#> |p                  | 0.378|
#> |stars              |      |
```
