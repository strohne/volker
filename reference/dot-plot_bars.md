# Helper function: plot grouped bar chart

Helper function: plot grouped bar chart

## Usage

``` r
.plot_bars(
  data,
  category = NULL,
  ci = FALSE,
  scale = NULL,
  limits = NULL,
  numbers = NULL,
  orientation = "horizontal",
  base = NULL,
  title = NULL
)
```

## Arguments

- data:

  Data frame with the columns item, value, p, n and optionally w If w is
  provided, the column width is generated according the w value,
  resulting in a mosaic plot.

- category:

  Optionally, a category to focus. All rows not matching the category
  will be filtered out.

- ci:

  Whether to plot error bars for 95% confidence intervals. Provide the
  columns ci.low and ci.high in data.

- scale:

  Direction of the scale: 0 = no direction for categories, -1 =
  descending or 1 = ascending values.

- numbers:

  Set to something that evaluates to TRUE and add the .values column to
  the data frame to ouput values on the bars.

- orientation:

  Whether to show bars (horizontal) or columns (vertical)

- base:

  The plot base as character or NULL.

- title:

  The plot title as character or NULL.

## Value

A ggplot object.
