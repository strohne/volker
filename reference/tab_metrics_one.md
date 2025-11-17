# Output a five point summary table for the values in multiple columns

Output a five point summary table for the values in multiple columns

## Usage

``` r
tab_metrics_one(
  data,
  col,
  ci = FALSE,
  digits = 1,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- col:

  The columns holding metric values.

- ci:

  Whether to calculate 95% confidence intervals of the mean.

- digits:

  The number of digits to print.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [tab_metrics](https://strohne.github.io/volker/reference/tab_metrics.md).

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt

tab_metrics_one(data, sd_age)
#> 
#> 
#> |Age    | value|
#> |:------|-----:|
#> |min    |    18|
#> |q1     |    27|
#> |median |    38|
#> |q3     |    52|
#> |max    |    68|
#> |mean   |  39.7|
#> |sd     |  13.8|
#> |n      |   101|
#> 
#> n=101.
#> 
```
