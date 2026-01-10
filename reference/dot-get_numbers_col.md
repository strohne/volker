# Determine the column used for plotting numbers

This function is used internally by plotting helpers such as
[`.plot_bars()`](https://strohne.github.io/volker/reference/dot-plot_bars.md)
and
[`.plot_heatmap()`](https://strohne.github.io/volker/reference/dot-plot_heatmap.md)
to avoid hard-coding the label column name.

## Usage

``` r
.get_numbers_col(numbers, col = ".values")
```

## Arguments

- numbers:

  The `numbers` parameter passed to the high-level plotting.

- col:

  Name of the column containing the prepared label values. Defaults to
  `".values"`.

## Value

A character string with the column name to be used for plotting numbers,
or `NULL` if no numbers should be displayed.
