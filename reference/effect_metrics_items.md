# Test whether a distribution is normal for each item

The test is calculated using
`stats::`[`shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html).

## Usage

``` r
effect_metrics_items(
  data,
  cols,
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

  The column holding metric values.

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

A volker table containing itemwise statistics:

- **skewness**: Measure of asymmetry in the distribution. A value of 0
  indicates perfect symmetry.

- **kurtosis**: Measure of the "tailedness" of the distribution.

- **W**: W-statistic from the Shapiro-Wilk normality test.

- **p**: p-value for the statistical test.

- **stars**: Significance stars based on p-value (\*, \*\*, \*\*\*).

- **normality**: Interpretation of normality based on Shapiro-Wilk test.

## Examples

``` r
library(volker)
data <- volker::chatgpt

effect_metrics_items(data, starts_with("cg_adoption"))
#> 
#> 
#> |Expectations                             | skewness| kurtosis|    W|     p| stars|  normality|
#> |:----------------------------------------|--------:|--------:|----:|-----:|-----:|----------:|
#> |ChatGPT has clear advantages compared... |    -0.51|    -0.05| 0.89| 0.000|   ***| not normal|
#> |Using ChatGPT brings financial benefits. |     0.09|    -1.00| 0.90| 0.000|   ***| not normal|
#> |Using ChatGPT is advantageous in many... |    -0.76|    -0.02| 0.86| 0.000|   ***| not normal|
#> |Compared to other systems, using Chat... |    -0.66|     0.43| 0.86| 0.000|   ***| not normal|
#> |Much can go wrong when using ChatGPT.    |     0.12|    -0.85| 0.91| 0.000|   ***| not normal|
#> |There are legal issues with using Cha... |     0.07|    -0.66| 0.90| 0.000|   ***| not normal|
#> |The security of user data is not guar... |     0.22|    -0.61| 0.89| 0.000|   ***| not normal|
#> |Using ChatGPT could bring personal di... |     0.33|    -0.65| 0.91| 0.000|   ***| not normal|
#> |In my environment, using ChatGPT is s... |     0.39|    -0.68| 0.90| 0.000|   ***| not normal|
#> |Almost everyone in my environment use... |     0.56|    -0.51| 0.88| 0.000|   ***| not normal|
#> |Not using ChatGPT is considered being... |     1.08|     0.22| 0.79| 0.000|   ***| not normal|
#> |Using ChatGPT brings me recognition f... |     0.57|    -0.77| 0.87| 0.000|   ***| not normal|
#> 
#> 4 missing case(s) omitted. Adjusted significance p values with fdr method.
#> 

```
