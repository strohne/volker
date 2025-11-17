# Helper function: plot grouped line chart by summarising values

Helper function: plot grouped line chart by summarising values

## Usage

``` r
.plot_summary(
  data,
  ci = FALSE,
  scale = NULL,
  base = NULL,
  box = FALSE,
  limits = NULL,
  title = NULL
)
```

## Arguments

- data:

  Dataframe with the columns item, value.

- ci:

  Whether to plot confidence intervals of the means.

- scale:

  Passed to the label scale function.

- base:

  The plot base as character or NULL.

- box:

  Whether to add boxplots.

- title:

  The plot title as character or NULL.

## Value

A ggplot object.
