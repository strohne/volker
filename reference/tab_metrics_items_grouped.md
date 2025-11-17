# Output the means for groups in one or multiple columns

Output the means for groups in one or multiple columns

## Usage

``` r
tab_metrics_items_grouped(
  data,
  cols,
  cross,
  digits = 1,
  values = c("m", "sd"),
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- cols:

  The item columns that hold the values to summarize.

- cross:

  The column holding groups to compare.

- digits:

  The number of digits to print.

- values:

  The output metrics, mean (m), the standard deviation (sd) or both (the
  default).

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [tab_metrics](https://strohne.github.io/volker/reference/tab_metrics.md).

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt

tab_metrics_items_grouped(data, starts_with("cg_adoption_"), sd_gender)
#> 
#> 
#> |Expectations                             |     total|    female|      male|  diverse|
#> |:----------------------------------------|---------:|---------:|---------:|--------:|
#> |ChatGPT has clear advantages compared... | 3.4 (1.0)| 3.6 (1.0)| 3.3 (1.0)| 4.0 (NA)|
#> |Using ChatGPT brings financial benefits. | 2.7 (1.2)| 2.6 (1.2)| 2.7 (1.2)| 3.0 (NA)|
#> |Using ChatGPT is advantageous in many... | 3.6 (1.1)| 3.7 (1.0)| 3.5 (1.1)| 4.0 (NA)|
#> |Compared to other systems, using Chat... | 3.5 (1.0)| 3.6 (1.0)| 3.5 (1.0)| 3.0 (NA)|
#> |Much can go wrong when using ChatGPT.    | 3.1 (1.1)| 3.1 (1.0)| 3.1 (1.2)| 3.0 (NA)|
#> |There are legal issues with using Cha... | 3.1 (1.2)| 3.0 (1.0)| 3.1 (1.3)| 3.0 (NA)|
#> |The security of user data is not guar... | 3.2 (1.0)| 3.0 (1.0)| 3.3 (1.1)| 3.0 (NA)|
#> |Using ChatGPT could bring personal di... | 2.7 (1.1)| 2.5 (0.9)| 2.8 (1.2)| 4.0 (NA)|
#> |In my environment, using ChatGPT is s... | 2.5 (1.1)| 2.5 (0.9)| 2.5 (1.3)| 4.0 (NA)|
#> |Almost everyone in my environment use... | 2.4 (1.2)| 2.4 (1.0)| 2.3 (1.3)| 4.0 (NA)|
#> |Not using ChatGPT is considered being... | 2.0 (1.2)| 1.8 (1.0)| 2.1 (1.3)| 4.0 (NA)|
#> |Using ChatGPT brings me recognition f... | 2.3 (1.2)| 2.4 (1.2)| 2.3 (1.3)| 3.0 (NA)|
#> |n                                        |        97|        37|        59|        1|
#> 
#> n=97. 4 missing case(s) omitted.
#> 
```
