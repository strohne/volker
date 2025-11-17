# Output a five point summary for groups

Output a five point summary for groups

## Usage

``` r
tab_metrics_one_grouped(
  data,
  col,
  cross,
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

  The column holding metric values.

- cross:

  The column holding groups to compare.

- ci:

  Whether to output 95% confidence intervals.

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

tab_metrics_one_grouped(data, sd_age, sd_gender)
#> 
#> 
#> |Gender  | min|   q1| median|   q3| max| mean|   sd|   n|
#> |:-------|---:|----:|------:|----:|---:|----:|----:|---:|
#> |female  |  18| 25.8|   38.0| 44.2|  63| 37.5| 13.4|  40|
#> |male    |  19| 32.5|   38.5| 52.0|  68| 41.2| 14.0|  60|
#> |diverse |  33| 33.0|   33.0| 33.0|  33| 33.0|     |   1|
#> |total   |  18| 27.0|   38.0| 52.0|  68| 39.7| 13.8| 101|
#> 
#> n=101.
#> 
```
