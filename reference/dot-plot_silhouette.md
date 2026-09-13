# Helper function: silhouette plot

Helper function: silhouette plot

## Usage

``` r
.plot_silhouette(data, k = NULL, lab_x = NULL, lab_y = NULL)
```

## Arguments

- data:

  Dataframe with the number of clusters k in the first column and the
  average silhouette width in the second.

- k:

  Provide one of the values in the first column to highlight the point
  at this value (the selected number of clusters).

- lab_x:

  Label of the x axis

- lab_y:

  Label of the y axis

## Value

A vlkr_plot object
