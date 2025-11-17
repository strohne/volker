# Output a correlation table for item battery and item battery

**\[experimental\]**

## Usage

``` r
tab_metrics_items_cor_items(
  data,
  cols,
  cross,
  method = "pearson",
  digits = 2,
  ci = FALSE,
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

- ci:

  Whether to calculate 95% confidence intervals of the correlation
  coefficient.

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

tab_metrics_items_cor_items(
  data,
  starts_with("cg_adoption_adv"),
  starts_with("use"),
  metric = TRUE
)
#> 
#> 
#> |Expectations                             |                   Usage| Pearson's r|
#> |:----------------------------------------|-----------------------:|-----------:|
#> |ChatGPT has clear advantages compared... |      in private context|        0.50|
#> |ChatGPT has clear advantages compared... | in professional context|        0.27|
#> |Using ChatGPT brings financial benefits. |      in private context|        0.17|
#> |Using ChatGPT brings financial benefits. | in professional context|        0.53|
#> |Using ChatGPT is advantageous in many... |      in private context|        0.34|
#> |Using ChatGPT is advantageous in many... | in professional context|        0.35|
#> |Compared to other systems, using Chat... |      in private context|        0.47|
#> |Compared to other systems, using Chat... | in professional context|        0.27|
#> 
#> n=99. 2 missing case(s) omitted. Adjusted significance p values with fdr method.
#> 
```
