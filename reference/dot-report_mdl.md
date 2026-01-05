# Generate an model table and plot

Generate an model table and plot

## Usage

``` r
.report_mdl(
  data,
  cols,
  categorical = NULL,
  metric = NULL,
  interactions = NULL,
  ...,
  title = TRUE
)
```

## Arguments

- data:

  A data frame.

- cols:

  A a single column (without quotes).

- categorical:

  A tidy column selection holding independet categorical variables.

- metric:

  A tidy column selection holding independent metric variables.

- interactions:

  A vector of interaction effects to calculate, passed to
  [`add_model()`](https://strohne.github.io/volker/reference/add_model.md).

- title:

  Add a plot title (default = TRUE).

## Value

A list containing a table and a plot volker report chunk.
