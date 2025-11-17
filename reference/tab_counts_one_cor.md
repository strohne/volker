# Count values by a metric column that will be split into groups

Count values by a metric column that will be split into groups

## Usage

``` r
tab_counts_one_cor(
  data,
  col,
  cross,
  prop = "total",
  percent = TRUE,
  values = c("n", "p"),
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- col:

  The column holding factor values.

- cross:

  The metric column that will be split into groups at the median.

- prop:

  The basis of percent calculation: "total" (the default), "cols", or
  "rows".

- percent:

  Proportions are formatted as percent by default. Set to FALSE to get
  bare proportions.

- values:

  The values to output: n (frequency) or p (percentage) or both (the
  default).

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

tab_counts_one_cor(data, adopter, sd_age)
#> 
#> 
#> |Innovator type                           |      total|  Low Age| High Age|
#> |:----------------------------------------|----------:|--------:|--------:|
#> |I try new offers immediately             |   15% (15)| 10% (10)|   5% (5)|
#> |I try new offers rather quickly          |   62% (63)| 24% (24)| 39% (39)|
#> |I wait until offers establish themselves |   22% (22)| 12% (12)| 10% (10)|
#> |I only use new offers when I have no ... |     1% (1)|   0% (0)|   1% (1)|
#> |total                                    | 100% (101)| 46% (46)| 54% (55)|
#> 
#> n=101. Age split at median 38.
#> 
```
