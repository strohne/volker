# Multiple items correlated with one metric variable

Multiple items correlated with one metric variable

## Usage

``` r
plot_metrics_items_cor(
  data,
  cols,
  cross,
  ci = FALSE,
  method = "pearson",
  title = TRUE,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble containing item measures.

- cols:

  Tidyselect item variables (e.g. starts_with...).

- cross:

  The column to correlate.

- ci:

  Whether to plot confidence intervals of the correlation coefficient.

- method:

  The method of correlation calculation, pearson = Pearson's R, spearman
  = Spearman's rho.

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

plot_metrics_items_cor(data, starts_with("use_"), sd_age)

```
