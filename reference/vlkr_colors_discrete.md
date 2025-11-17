# Get colors for discrete scales

If the option ggplot2.discrete.fill is set, gets color values from the
first list item that has enough colors and reverses them to start
filling from the left in grouped bar charts.

## Usage

``` r
vlkr_colors_discrete(n = NULL, inv = FALSE)
```

## Arguments

- n:

  Number of colors.

- inv:

  Whether to get a text color with good contrast on the chosen fill
  colors.

## Value

A vector of colors.

## Details

Falls back to scale_fill_hue().
