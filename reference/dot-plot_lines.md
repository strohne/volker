# Helper function: plot grouped line chart

Helper function: plot grouped line chart

## Usage

``` r
.plot_lines(
  data,
  reorder = FALSE,
  scale = NULL,
  base = NULL,
  limits = NULL,
  title = NULL
)
```

## Arguments

- data:

  Dataframe with the columns item, value, and .cross

- reorder:

  Reorder items to minimize line crossings. Either `TRUE` to
  automatically select a method (`"olo"` if seriation is installed,
  otherwise `"min"`), or one of the character values `"max"`, `"min"`,
  `"spread"`, `"gw"`, or `"olo"`. Defaults to `FALSE` which disables
  reordering.

- scale:

  Passed to the label scale function.

- base:

  The plot base as character or NULL.

- limits:

  The scale limits.

- title:

  The plot title as character or NULL.

## Value

A ggplot object.
