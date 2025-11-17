# Output frequencies cross tabulated with a grouping column

Output frequencies cross tabulated with a grouping column

## Usage

``` r
tab_counts_one_grouped(
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

  The column holding groups to split.

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

tab_counts_one_grouped(data, adopter, sd_gender)
#> 
#> 
#> |Innovator type                           |      total|   female|     male| diverse|
#> |:----------------------------------------|----------:|--------:|--------:|-------:|
#> |I try new offers immediately             |   15% (15)|   2% (2)| 12% (12)|  1% (1)|
#> |I try new offers rather quickly          |   62% (63)| 25% (25)| 38% (38)|  0% (0)|
#> |I wait until offers establish themselves |   22% (22)| 13% (13)|   9% (9)|  0% (0)|
#> |I only use new offers when I have no ... |     1% (1)|   0% (0)|   1% (1)|  0% (0)|
#> |total                                    | 100% (101)| 40% (40)| 59% (60)|  1% (1)|
#> 
#> n=101.
#> 
```
