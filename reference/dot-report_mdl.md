# Generate an model table and plot

Generate an model table and plot

## Usage

``` r
.report_mdl(data, cols, categorical = NULL, metric = NULL, ..., title = TRUE)
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

- title:

  Add a plot title (default = TRUE).

## Value

A list containing a table and a plot volker report chunk.
