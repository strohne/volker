# Plot the frequency of values in one column

Plot the frequency of values in one column

## Usage

``` r
plot_counts_one(
  data,
  col,
  category = NULL,
  ci = FALSE,
  limits = NULL,
  numbers = NULL,
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

  The column holding values to count.

- category:

  The value FALSE will force to plot all categories. A character value
  will focus a selected category. When NULL, in case of boolean values,
  only the TRUE category is plotted.

- ci:

  Whether to plot error bars for 95% confidence intervals.

- limits:

  The scale limits, autoscaled by default. Set to `c(0,100)` to make a
  100% plot. If the data is binary or focused on a single category, by
  default a 100% plot is created.

- numbers:

  The values to print on the bars: "n" (frequency), "p" (percentage) or
  both.

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
  [plot_counts](https://strohne.github.io/volker/reference/plot_counts.md).

## Value

A ggplot object.

## Examples

``` r
library(volker)
data <- volker::chatgpt

plot_counts_one(data, sd_gender)

```
