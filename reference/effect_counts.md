# Output effect sizes and test statistics for count data

The type of effect size depends on the number of selected columns:

- One categorical column: see
  [effect_counts_one](https://strohne.github.io/volker/reference/effect_counts_one.md)

- Multiple categorical columns: see
  [effect_counts_items](https://strohne.github.io/volker/reference/effect_counts_items.md)

Cross tabulations:

- One categorical column and one grouping column: see
  [effect_counts_one_grouped](https://strohne.github.io/volker/reference/effect_counts_one_grouped.md)

- Multiple categorical columns and one grouping column: see
  [effect_counts_items_grouped](https://strohne.github.io/volker/reference/effect_counts_items_grouped.md)
  (not yet implemented)

- Multiple categorical columns and multiple grouping columns:
  [effect_counts_items_grouped_items](https://strohne.github.io/volker/reference/effect_counts_items_grouped_items.md)
  (not yet implemented)

By default, if you provide two column selections, the second column is
treated as categorical. Setting the metric-parameter to TRUE will call
the appropriate functions for correlation analysis:

- One categorical column and one metric column: see
  [effect_counts_one_cor](https://strohne.github.io/volker/reference/effect_counts_one_cor.md)
  (not yet implemented)

- Multiple categorical columns and one metric column: see
  [effect_counts_items_cor](https://strohne.github.io/volker/reference/effect_counts_items_cor.md)
  (not yet implemented)

- Multiple categorical columns and multiple metric
  columns:[effect_counts_items_cor_items](https://strohne.github.io/volker/reference/effect_counts_items_cor_items.md)
  (not yet implemented)

**\[experimental\]**

## Usage

``` r
effect_counts(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...)
```

## Arguments

- data:

  A data frame.

- cols:

  A tidy column selection, e.g. a single column (without quotes) or
  multiple columns selected by methods such as starts_with().

- cross:

  Optional, a grouping column. The column name without quotes.

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

effect_counts(data, sd_gender, adopter)
#> 
#> 
#> |sd_gender |                                  adopter|  n|  p_x|  p_y| p_xy| ratio|   pmi|  npmi| fisher_p| fisher_stars|
#> |:---------|----------------------------------------:|--:|----:|----:|----:|-----:|-----:|-----:|--------:|------------:|
#> |female    |             I try new offers immediately|  2| 0.40| 0.15| 0.02|  0.34| -1.57| -0.28|    0.142|             |
#> |female    |          I try new offers rather quickly| 25| 0.40| 0.62| 0.25|  1.00|  0.00|  0.00|    1.000|             |
#> |female    | I wait until offers establish themselves| 13| 0.40| 0.22| 0.13|  1.49|  0.58|  0.20|    0.142|             |
#> |male      |             I try new offers immediately| 12| 0.59| 0.15| 0.12|  1.35|  0.43|  0.14|    0.187|             |
#> |male      |          I try new offers rather quickly| 38| 0.59| 0.62| 0.38|  1.02|  0.02|  0.02|    1.000|             |
#> |male      | I wait until offers establish themselves|  9| 0.59| 0.22| 0.09|  0.69| -0.54| -0.15|    0.142|             |
#> |male      | I only use new offers when I have no ...|  1| 0.59| 0.01| 0.01|  1.68|  0.75|  0.11|    1.000|             |
#> |diverse   |             I try new offers immediately|  1| 0.01| 0.15| 0.01|  6.73|  2.75|  0.41|    0.238|             |
#> 
#> Adjusted significance p values with fdr method.
#> 
#> 
#> 
#> |Statistic   | Value|
#> |:-----------|-----:|
#> |Cramer's V  |  0.26|
#> |Chi-squared | 13.48|
#> |n           |   101|
#> |df          |      |
#> |p           | 0.019|
#> |stars       |     *|
```
