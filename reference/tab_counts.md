# Output a frequency table

The type of frequency table depends on the number of selected columns:

- One categorical column: see
  [tab_counts_one](https://strohne.github.io/volker/reference/tab_counts_one.md)

- Multiple categorical columns: see
  [tab_counts_items](https://strohne.github.io/volker/reference/tab_counts_items.md)

Cross tabulations:

- One categorical column and one grouping column: see
  [tab_counts_one_grouped](https://strohne.github.io/volker/reference/tab_counts_one_grouped.md)

- Multiple categorical columns and one grouping column: see
  [tab_counts_items_grouped](https://strohne.github.io/volker/reference/tab_counts_items_grouped.md)

- Multiple categorical columns and multiple grouping columns: see
  [tab_counts_items_grouped_items](https://strohne.github.io/volker/reference/tab_counts_items_grouped_items.md)
  (not yet implemented)

By default, if you provide two column selections, the second column is
treated as categorical. Setting the metric-parameter to TRUE will call
the appropriate functions for correlation analysis:

- One categorical column and one metric column: see
  [tab_counts_one_cor](https://strohne.github.io/volker/reference/tab_counts_one_cor.md)

- Multiple categorical columns and one metric column: see
  [tab_counts_items_cor](https://strohne.github.io/volker/reference/tab_counts_items_cor.md)

- Multiple categorical columns and multiple metric columns:
  [tab_counts_items_cor_items](https://strohne.github.io/volker/reference/tab_counts_items_cor_items.md)
  (not yet implemented)

Parameters that may be passed to specific count functions:

- **ci**: Add confidence intervals to proportions.

- **percent**: Frequency tables show percentages by default. Set to
  FALSE to get raw proportions.

- **prop**: For cross tables you can choose between total, row or column
  percentages.

- **values**: The values to output: n (frequency) or p (percentage) or
  both (the default).

- **category**: When you have multiple categories in a column, you can
  focus one of the categories to simplify the plots. By default, if a
  column has only TRUE and FALSE values, the outputs focus the TRUE
  category.

- **labels**: Labels are extracted from the column attributes. Set to
  FALSE to output bare column names and values.

**\[experimental\]**

## Usage

``` r
tab_counts(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...)
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

  Other parameters passed to the appropriate table function.

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt

tab_counts(data, sd_gender)
#> 
#> 
#> |Gender  |   n|    p|
#> |:-------|---:|----:|
#> |female  |  40|  40%|
#> |male    |  60|  59%|
#> |diverse |   1|   1%|
#> |total   | 101| 100%|
#> 
#> n=101.
#> 
```
