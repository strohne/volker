# Output effect sizes and test statistics for metric data

The calculations depend on the number of selected columns:

- One metric column: see
  [effect_metrics_one](https://strohne.github.io/volker/reference/effect_metrics_one.md)

- Multiple metric columns: see
  [effect_metrics_items](https://strohne.github.io/volker/reference/effect_metrics_items.md)

Group comparisons:

- One metric column and one grouping column: see
  [effect_metrics_one_grouped](https://strohne.github.io/volker/reference/effect_metrics_one_grouped.md)

- Multiple metric columns and one grouping column: see
  [effect_metrics_items_grouped](https://strohne.github.io/volker/reference/effect_metrics_items_grouped.md)

- Multiple metric columns and multiple grouping columns: not yet
  implemented

By default, if you provide two column selections, the second column is
treated as categorical. Setting the metric-parameter to TRUE will call
the appropriate functions for correlation analysis:

- Two metric columns: see
  [effect_metrics_one_cor](https://strohne.github.io/volker/reference/effect_metrics_one_cor.md)

- Multiple metric columns and one metric column: see
  [effect_metrics_items_cor](https://strohne.github.io/volker/reference/effect_metrics_items_cor.md)

- Two metric column selections: see
  [effect_metrics_items_cor_items](https://strohne.github.io/volker/reference/effect_metrics_items_cor_items.md)

**\[experimental\]**

## Usage

``` r
effect_metrics(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...)
```

## Arguments

- data:

  A data frame.

- cols:

  A tidy column selection, e.g. a single column (without quotes) or
  multiple columns selected by methods such as starts_with().

- cross:

  Optional, a grouping column (without quotes).

- metric:

  When crossing variables, the cross column parameter can contain
  categorical or metric values. By default, the cross column selection
  is treated as categorical data. Set metric to TRUE, to treat it as
  metric and calculate correlations.

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Other parameters passed to the appropriate effect function.

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt

effect_metrics(data, sd_age, sd_gender)
#> 
#> 
#> |Term               | estimate| ci low| ci high|    se|     t|     p| stars|
#> |:------------------|--------:|------:|-------:|-----:|-----:|-----:|-----:|
#> |(Intercept)        |    37.53|  33.21|   41.84|  2.18| 17.24| 0.000|   ***|
#> |female (Reference) |         |       |        |      |      |      |      |
#> |male               |     3.69|  -1.88|    9.27|  2.81|  1.31| 0.288|      |
#> |diverse            |    -4.52| -32.18|   23.13| 13.94| -0.32| 0.746|      |
#> 
#> Adjusted significance p values with fdr method.
#> 
#> 
#> 
#> |Statistic          | Value|
#> |:------------------|-----:|
#> |Adjusted R-squared | -0.00|
#> |F                  |  0.98|
#> |df                 |     2|
#> |residual df        |    98|
#> |p                  | 0.378|
#> |stars              |      |
```
