# Printing method for volker plots when knitting

Printing method for volker plots when knitting

## Usage

``` r
# S3 method for class 'vlkr_plt'
knit_print(x, ...)
```

## Arguments

- x:

  The volker plot.

- ...:

  Further parameters passed to print().

## Value

Knitr asis output

## Examples

``` r
library(volker)
data <- volker::chatgpt

pl <- plot_metrics(data, sd_age)
print(pl)

```
