# Helper function: plot cor and regression outputs

Helper function: plot cor and regression outputs

## Usage

``` r
.plot_cor(
  data,
  ci = TRUE,
  base = NULL,
  limits = NULL,
  title = NULL,
  label = NULL
)
```

## Arguments

- data:

  Dataframe with the columns item and value. To plot errorbars, add the
  columns low and high and set the ci-paramater to TRUE.

- ci:

  Whether to plot confidence intervals. Provide the columns low and high
  in data.

- base:

  The plot base as character or NULL.

- limits:

  The scale limits.

- title:

  The plot title as character or NULL.

- label:

  The y axis label.

## Value

A ggplot object.
