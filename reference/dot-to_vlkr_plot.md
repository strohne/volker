# Add the volker class and options

Add the volker class and options

## Usage

``` r
.to_vlkr_plot(
  pl,
  rows = NULL,
  maxlab = NULL,
  baseline = TRUE,
  theme_options = TRUE
)
```

## Arguments

- pl:

  A ggplot object.

- rows:

  The number of items on the vertical axis. Will be automatically
  determined when NULL. For stacked bar charts, don't forget to set the
  group parameter, otherwise it won't work

- maxlab:

  The character length of the longest label to be plotted. Will be
  automatically determined when NULL. on the vertical axis.

- baseline:

  Whether to print a message about removed values.

- theme_options:

  Enable or disable axis titles and text, by providing a list with any
  of the elements axis.text.x, axis.text.y, axis.title.x, axis.title.y
  set to TRUE or FALSE. By default, titles (=scale labels) are disabled
  and text (= the tick labels) are enabled. Enable or disable legend
  title by setting the list element legend.title to TRUE or FALSE.
  Legend titles are disabled by default.

## Value

A ggplot object with vlkr_plt class.
