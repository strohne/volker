# Printing method for volker plots

Printing method for volker plots

## Usage

``` r
# S3 method for class 'vlkr_plt'
print(x, ...)

# S3 method for class 'vlkr_plt'
plot(x, ...)
```

## Arguments

- x:

  The volker plot.

- ...:

  Further parameters passed to print().

## Value

No return value.

## Examples

``` r
library(volker)
data <- volker::chatgpt

pl <- plot_metrics(data, sd_age)
print(pl)

```
