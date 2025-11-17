# Output averages for multiple variables

Output averages for multiple variables

## Usage

``` r
plot_metrics_items(
  data,
  cols,
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

  A tibble containing item measures.

- cols:

  Tidyselect item variables (e.g. starts_with...).

- ci:

  Whether to plot the 95% confidence interval of the mean.

- box:

  Whether to add boxplots.

- limits:

  The scale limits. Set NULL to extract limits from the labels. NOT
  IMPLEMENTED YET.

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

plot_metrics_items(data, starts_with("cg_adoption_"))

#> In the plot, 4 missing case(s) omitted.
```
