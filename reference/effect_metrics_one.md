# Test whether a distribution is normal

The test is calculated using
`stats::`[`shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html).

## Usage

``` r
effect_metrics_one(data, col, labels = TRUE, clean = TRUE, ...)
```

## Arguments

- data:

  A tibble.

- col:

  The column holding metric values.

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

A volker list object with the following statistical measures:

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

effect_metrics_one(data, sd_age)
#> 
#> 
#> |Age      | Value|
#> |:--------|-----:|
#> |skewness |  0.25|
#> |kurtosis | -0.98|
#> 
#> 
#> |Shapiro-Wilk normality test |      Value|
#> |:---------------------------|----------:|
#> |W                           |       0.96|
#> |p                           |      0.002|
#> |stars                       |         **|
#> |normality                   | not normal|
```
