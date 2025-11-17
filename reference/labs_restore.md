# Restore labels from the codebook store in the codebook attribute.

**\[experimental\]**

## Usage

``` r
labs_restore(data, cols = NULL)
```

## Arguments

- data:

  A data frame.

- cols:

  A tidyselect column selection.

## Value

A data frame.

## Details

You can store labels before mutate operations by calling
[labs_store](https://strohne.github.io/volker/reference/labs_store.md).

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
