# Generate an index table and plot

Generate an index table and plot

## Usage

``` r
.report_idx(
  data,
  cols,
  cross,
  metric = FALSE,
  ...,
  effect = FALSE,
  title = TRUE
)
```

## Arguments

- data:

  A data frame.

- cols:

  A tidy column selection, e.g. a single column (without quotes) or
  multiple columns selected by methods such as starts_with().

- cross:

  Optional, a grouping column (without quotes).

- metric:

  When crossing variables, the cross column parameter can contain
  categorical or metric values. By default, the cross column selection
  is treated as categorical data. Set metric to TRUE, to treat it as
  metric and calculate correlations.

- effect:

  Whether to report statistical tests and effect sizes.

- title:

  Add a plot title (default = TRUE).

## Value

A list containing a table and a plot volker report chunk.
