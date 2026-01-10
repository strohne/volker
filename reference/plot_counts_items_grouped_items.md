# Correlation of categorical items with categorical items

Correlation of categorical items with categorical items

## Usage

``` r
plot_counts_items_grouped_items(
  data,
  cols,
  cross,
  method = "cramer",
  numbers = TRUE,
  category = NULL,
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

  Tidyselect item variables (e.g. starts_with...).

- method:

  The method of correlation calculation:

  - `cramer` for Cramer's V,

  - `npmi` for Normalized Pointwise Mutual Information.

- numbers:

  Whether to print the association values on the tiles. Default is
  `TRUE`.

- category:

  The value FALSE will force to plot all categories. A character value
  will focus a selected category. When NULL, in case of boolean values,
  only the TRUE category is plotted.

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

plot_counts_items_grouped_items(
  data,
  starts_with("cg_adoption_advantage"),
  starts_with("cg_adoption_fearofuse"),
  method = "cramer"
)

```
