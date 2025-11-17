# Correlate two items

Correlate two items

## Usage

``` r
plot_metrics_one_cor(
  data,
  col,
  cross,
  limits = NULL,
  log = FALSE,
  jitter = FALSE,
  title = TRUE,
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

- limits:

  The scale limits, a list with x and y components, e.g.
  `list(x=c(0,100), y=c(20,100))`. Set NULL to extract limits from the
  labels.

- log:

  Whether to plot log scales.

- jitter:

  Whether to jitter the points.

- title:

  If TRUE (default) shows a plot title derived from the column labels.
  Disable the title with FALSE or provide a custom title as character
  value.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [plot_metrics](https://strohne.github.io/volker/reference/plot_metrics.md).

## Value

A ggplot object.

## Examples

``` r
library(volker)
data <- volker::chatgpt

plot_metrics_one_cor(data, use_private, sd_age)

```
