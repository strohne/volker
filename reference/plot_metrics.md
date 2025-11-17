# Output a plot with distribution parameters such as the mean values

The plot type depends on the number of selected columns:

- One metric column: see
  [plot_metrics_one](https://strohne.github.io/volker/reference/plot_metrics_one.md)

- Multiple metric columns: see
  [plot_metrics_items](https://strohne.github.io/volker/reference/plot_metrics_items.md)

Group comparisons:

- One metric column and one grouping column: see
  [plot_metrics_one_grouped](https://strohne.github.io/volker/reference/plot_metrics_one_grouped.md)

- Multiple metric columns and one grouping column: see
  [plot_metrics_items_grouped](https://strohne.github.io/volker/reference/plot_metrics_items_grouped.md)

- Multiple metric columns and multiple grouping columns: see
  [plot_metrics_items_grouped_items](https://strohne.github.io/volker/reference/plot_metrics_items_grouped_items.md)
  (not yet implemented)

By default, if you provide two column selections, the second selection
is treated as categorical. Setting the metric-parameter to TRUE will
call the appropriate functions for correlation analysis:

- Two metric columns: see
  [plot_metrics_one_cor](https://strohne.github.io/volker/reference/plot_metrics_one_cor.md)

- Multiple metric columns and one metric column : see
  [plot_metrics_items_cor](https://strohne.github.io/volker/reference/plot_metrics_items_cor.md)

- Two metric column selections: see
  [plot_metrics_items_cor_items](https://strohne.github.io/volker/reference/plot_metrics_items_cor_items.md)

Parameters that may be passed to the metric functions (see the
respective function help):

- **ci**: Plot confidence intervals for means or correlation
  coefficients.

- **box**: Visualise the distribution by adding boxplots.

- **log**: In scatter plots, you can use a logarithmic scale. Be aware,
  that zero values will be omitted because their log value is undefined.

- **method**: By default, correlations are calculated using Pearson’s R.
  You can choose Spearman’s Rho with the methods-parameter.

- **limits**: The scale limits are automatically guessed by the package
  functions (work in progress). Use the limits-parameter to manually fix
  any misleading graphs.

- **title**: All plots usually get a title derived from the column
  attributes or column names. Set to FALSE to suppress the title or
  provide a title of your choice as a character value.

- **labels**: Labels are extracted from the column attributes. Set to
  FALSE to output bare column names and values.

- **numbers**: Controls whether to display correlation coefficients on
  the plot.

**\[experimental\]**

## Usage

``` r
plot_metrics(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...)
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

  Other parameters passed to the appropriate plot function.

## Value

A ggplot object.

## Examples

``` r
library(volker)
data <- volker::chatgpt

plot_metrics(data, sd_age)

```
