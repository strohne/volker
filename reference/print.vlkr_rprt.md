# Printing method for volker reports

Printing method for volker reports

## Usage

``` r
# S3 method for class 'vlkr_rprt'
print(x, ...)
```

## Arguments

- x:

  The volker report object.

- ...:

  Further parameters passed to print.

## Value

No return value.

## Examples

``` r
library(volker)
data <- volker::chatgpt

rp <- report_metrics(data, sd_age)
print(rp)

#> 
#> 
#> |Age    | value|
#> |:------|-----:|
#> |min    |    18|
#> |q1     |    27|
#> |median |    38|
#> |q3     |    52|
#> |max    |    68|
#> |mean   |  39.7|
#> |sd     |  13.8|
#> |n      |   101|
#> 
#> n=101.
#> 
```
