# Correlate two columns

Correlate two columns

## Usage

``` r
tab_metrics_one_cor(
  data,
  col,
  cross,
  method = "pearson",
  ci = FALSE,
  digits = 2,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- col:

  The first column holding metric values.

- cross:

  The second column holding metric values.

- method:

  The output metrics, TRUE or pearson = Pearson's R, spearman =
  Spearman's rho

- ci:

  Whether to output confidence intervals.

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
  [tab_counts](https://strohne.github.io/volker/reference/tab_counts.md).

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt

tab_metrics_one_cor(data, use_private, sd_age)
#> 
#> 
#> |Item 1                    | Item 2|   n| Pearson's r|
#> |:-------------------------|------:|---:|-----------:|
#> |Usage: in private context |    Age| 101|       -0.19|
#> 
#> n=101.
#> 
```
