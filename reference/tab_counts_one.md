# Output a frequency table for the values in one column

Output a frequency table for the values in one column

## Usage

``` r
tab_counts_one(
  data,
  col,
  ci = FALSE,
  percent = TRUE,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- col:

  The column holding values to count.

- ci:

  Whether to compute 95% confidence intervals using
  `stats::`[`prop.test`](https://rdrr.io/r/stats/prop.test.html).

- percent:

  Proportions are formatted as percent by default. Set to FALSE to get
  bare proportions.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [tab_counts](https://strohne.github.io/volker/reference/tab_counts.md).

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt

tab_counts_one(data, sd_gender)
#> 
#> 
#> |Gender  |   n|    p|
#> |:-------|---:|----:|
#> |female  |  40|  40%|
#> |male    |  60|  59%|
#> |diverse |   1|   1%|
#> |total   | 101| 100%|
#> 
#> n=101.
#> 
```
