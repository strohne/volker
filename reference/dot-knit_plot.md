# Knit volker plots

Automatically calculates the plot height from chunk options and volker
options.

## Usage

``` r
.knit_plot(pl)
```

## Arguments

- pl:

  A ggplot object with vlkr_options. The vlk_options are added by
  .to_vlkr_plot() and provide information about the number of vertical
  items (rows) and the maximum.

## Value

Character string containing a html image tag, including the base64
encoded image.

## Details

Presumptions:

- a screen resolution of 72dpi

- a default plot width of 7 inches = 504px

- a default page width of 700px (vignette) or 910px (report)

- an optimal bar height of 40px for 910px wide plots. i.e. a ratio of
  0.04

- an offset of one bar above and one bar below
