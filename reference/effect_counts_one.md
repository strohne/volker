# Test homogeneity of category shares

Performs a goodness-of-fit test and calculates the Gini coefficient. The
goodness-of-fit-test is calculated using
`stats::`[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html).

## Usage

``` r
effect_counts_one(data, col, clean = TRUE, ...)
```

## Arguments

- data:

  A tibble.

- col:

  The column holding factor values.

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md)

- ...:

  Placeholder to allow calling the method with unused parameters from
  [effect_counts](https://strohne.github.io/volker/reference/effect_counts.md).

## Value

A volker tibble with the following statistical measures:

- **Gini coefficient**: Gini coefficient, measuring inequality.

- **n**: Number of cases the calculation is based on.

- **Chi-squared**: Chi-Squared test statistic.

- **p**: p-value for the statistical test.

- **stars**: Significance stars based on p-value (\*, \*\*, \*\*\*).

## Examples

``` r
library(volker)
data <- volker::chatgpt

data |>
  filter(sd_gender != "diverse") |>
  effect_counts_one(sd_gender)
#> 
#> 
#> |Statistic        | Value|
#> |:----------------|-----:|
#> |Gini coefficient |  0.10|
#> |n                |   100|
#> |Chi-squared      |  4.00|
#> |p                | 0.046|
#> |stars            |     *|
```
