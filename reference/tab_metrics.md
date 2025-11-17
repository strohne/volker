# Output a table with distribution parameters

The table type depends on the number of selected columns:

- One metric column: see
  [tab_metrics_one](https://strohne.github.io/volker/reference/tab_metrics_one.md)

- Multiple metric columns: see
  [tab_metrics_items](https://strohne.github.io/volker/reference/tab_metrics_items.md)

Group comparisons:

- One metric column and one grouping column: see
  [tab_metrics_one_grouped](https://strohne.github.io/volker/reference/tab_metrics_one_grouped.md)

- Multiple metric columns and one grouping column: see
  [tab_metrics_items_grouped](https://strohne.github.io/volker/reference/tab_metrics_items_grouped.md)

- Multiple metric columns and multiple grouping columns: see
  [tab_metrics_items_grouped_items](https://strohne.github.io/volker/reference/tab_metrics_items_grouped_items.md)
  (not yet implemented)

By default, if you provide two column selections, the second column is
treated as categorical. Setting the metric-parameter to TRUE will call
the appropriate functions for correlation analysis:

- Two metric columns: see
  [tab_metrics_one_cor](https://strohne.github.io/volker/reference/tab_metrics_one_cor.md)

- Multiple metric columns and one metric column: see
  [tab_metrics_items_cor](https://strohne.github.io/volker/reference/tab_metrics_items_cor.md)

- Two metric column selections: see
  [tab_metrics_items_cor_items](https://strohne.github.io/volker/reference/tab_metrics_items_cor_items.md)

Parameters that may be passed to specific metric functions:

- **ci**: Add confidence intervals for means or correlation
  coefficients.

- **values**: The output metrics, mean (m), the standard deviation (sd)
  or both (the default).

- **digits**: Tables containing means and standard deviations by default
  round values to one digit. Increase the number to show more digits

- **method**: By default, correlations are calculated using Pearson’s R.
  You can choose Spearman’s Rho with the methods-parameter.

- **labels**: Labels are extracted from the column attributes. Set to
  FALSE to output bare column names and values.

**\[experimental\]**

## Usage

``` r
tab_metrics(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...)
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

  Other parameters passed to the appropriate table function.

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt

tab_metrics(data, sd_age)
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
