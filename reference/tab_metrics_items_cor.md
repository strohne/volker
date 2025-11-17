# Output a correlation table for item battery and one metric variable

**\[experimental\]**

## Usage

``` r
tab_metrics_items_cor(
  data,
  cols,
  cross,
  method = "pearson",
  digits = 2,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- cols:

  The source columns.

- cross:

  The target columns or NULL to calculate correlations within the source
  columns.

- method:

  The output metrics, pearson = Pearson's R, spearman = Spearman's rho.

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

tab_metrics_items_cor(
  data,
  starts_with("cg_adoption_adv"),
  sd_age,
  metric = TRUE
)
#> 
#> 
#> |Expectations                             |   Age|
#> |:----------------------------------------|-----:|
#> |ChatGPT has clear advantages compared... | -0.12|
#> |Using ChatGPT brings financial benefits. | -0.09|
#> |Using ChatGPT is advantageous in many... | -0.06|
#> |Compared to other systems, using Chat... | -0.12|
#> 
#> n=99. 2 missing case(s) omitted.
#> 
```
