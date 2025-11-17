# Output a density plot for a single metric variable

Output a density plot for a single metric variable

## Usage

``` r
plot_metrics_one(
  data,
  col,
  ci = FALSE,
  box = FALSE,
  limits = NULL,
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

  The column holding metric values.

- ci:

  Whether to plot the confidence interval.

- box:

  Whether to add a boxplot.

- limits:

  The scale limits. Set NULL to extract limits from the label.

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

plot_metrics_one(data, sd_age)

```
