# Get the current codebook and store it in the codebook attribute.

**\[experimental\]**

## Usage

``` r
labs_store(data)
```

## Arguments

- data:

  A data frame.

## Value

A data frame.

## Details

You can restore the labels after mutate operations by calling
[labs_restore](https://strohne.github.io/volker/reference/labs_restore.md).

## Examples

``` r
library(dplyr)
library(volker)

volker::chatgpt |>
  labs_store() |>
  mutate(sd_age = 2024 - sd_age) |>
  labs_restore() |>
  tab_metrics(sd_age)
#> 
#> 
#> |Age    |  value|
#> |:------|------:|
#> |min    |   1956|
#> |q1     |   1972|
#> |median |   1986|
#> |q3     |   1997|
#> |max    |   2006|
#> |mean   | 1984.3|
#> |sd     |   13.8|
#> |n      |    101|
#> 
#> n=101.
#> 
```
