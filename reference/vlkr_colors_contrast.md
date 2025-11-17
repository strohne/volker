# Given a vector of hex fill colors, choose an appropriate color for text

Given a vector of hex fill colors, choose an appropriate color for text

## Usage

``` r
vlkr_colors_contrast(colors, threshold = 0.5)
```

## Arguments

- colors:

  A vector of hex colors

- threshold:

  Luminance theshold for choosing black over white

## Value

A vector of the same length as fill_colors with white or black colors.
