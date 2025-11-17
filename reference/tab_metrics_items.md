# Output a five point summary table for multiple items

Output a five point summary table for multiple items

## Usage

``` r
tab_metrics_items(
  data,
  cols,
  ci = FALSE,
  digits = 1,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- cols:

  The columns holding metric values.

- ci:

  Whether to compute confidence intervals of the mean.

- digits:

  The number of digits to print.

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

tab_metrics_items(data, starts_with("cg_adoption_"))
#> 
#> 
#> |Expectations                             | min| q1| median| q3| max| mean|  sd|  n|
#> |:----------------------------------------|---:|--:|------:|--:|---:|----:|---:|--:|
#> |ChatGPT has clear advantages compared... |   1|  3|      4|  4|   5|  3.4| 1.0| 97|
#> |Using ChatGPT brings financial benefits. |   1|  2|      3|  4|   5|  2.7| 1.2| 97|
#> |Using ChatGPT is advantageous in many... |   1|  3|      4|  4|   5|  3.6| 1.1| 97|
#> |Compared to other systems, using Chat... |   1|  3|      4|  4|   5|  3.5| 1.0| 97|
#> |Much can go wrong when using ChatGPT.    |   1|  2|      3|  4|   5|  3.1| 1.1| 97|
#> |There are legal issues with using Cha... |   1|  2|      3|  4|   5|  3.1| 1.2| 97|
#> |The security of user data is not guar... |   1|  3|      3|  4|   5|  3.2| 1.0| 97|
#> |Using ChatGPT could bring personal di... |   1|  2|      3|  3|   5|  2.7| 1.1| 97|
#> |In my environment, using ChatGPT is s... |   1|  2|      2|  3|   5|  2.5| 1.1| 97|
#> |Almost everyone in my environment use... |   1|  1|      2|  3|   5|  2.4| 1.2| 97|
#> |Not using ChatGPT is considered being... |   1|  1|      2|  3|   5|  2.0| 1.2| 97|
#> |Using ChatGPT brings me recognition f... |   1|  1|      2|  3|   5|  2.3| 1.2| 97|
#> 
#> n=97. 4 missing case(s) omitted.
#> 
```
