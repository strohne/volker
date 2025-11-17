# Define a default theme for volker plots

Set ggplot colors, sizes and layout parameters.

## Usage

``` r
theme_vlkr(
  base_size = 11,
  base_color = "black",
  base_fill = VLKR_FILLDISCRETE,
  base_gradient = VLKR_FILLGRADIENT
)
```

## Arguments

- base_size:

  Base font size.

- base_color:

  Base font color.

- base_fill:

  A list of fill color sets or at least one fill color set. Example:
  `list(c("red"), c("red", "blue", "green"))`. Each set can contain
  different numbers of colors. Depending on the number of colors needed,
  the set with at least the number of required colors is used. The first
  color is always used for simple bar charts.

- base_gradient:

  A color vector used for creating gradient fill colors, e.g. in stacked
  bar plots.

## Value

A theme function.

## Details

**\[experimental\]**

## Examples

``` r
library(volker)
library(ggplot2)
data <- volker::chatgpt

theme_set(theme_vlkr(base_size=15, base_fill = list("red")))
plot_counts(data, sd_gender)
```
