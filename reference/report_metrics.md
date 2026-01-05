# Create table and plot for metric variables

Depending on your column selection, different types of plots and tables
are generated. See
[plot_metrics](https://strohne.github.io/volker/reference/plot_metrics.md)
and
[tab_metrics](https://strohne.github.io/volker/reference/tab_metrics.md).

## Usage

``` r
report_metrics(
  data,
  cols,
  cross = NULL,
  metric = FALSE,
  interactions = NULL,
  ...,
  index = FALSE,
  factors = FALSE,
  clusters = FALSE,
  model = FALSE,
  effect = FALSE,
  title = TRUE,
  close = TRUE,
  clean = TRUE
)
```

## Arguments

- data:

  A data frame.

- cols:

  A tidy column selection, e.g. a single column (without quotes) or
  multiple columns selected by methods such as
  [`starts_with()`](https://strohne.github.io/volker/reference/starts_with.md).
  To calculate linear models, you can provide a formula and skip the
  parameters for independent variables, see the `model` parameter.

- cross:

  Optional, a grouping or correlation column (without quotes).

- metric:

  When crossing variables, the cross column parameter can contain
  categorical or metric values. By default, the cross column selection
  is treated as categorical data. Set metric to `TRUE`, to treat it as
  metric and calculate correlations. Alternatively, for multivariable
  models (if the model parameter is `TRUE`), provide the metric column
  selection in the metric parameter and the categorical column selection
  in the cross parameter.

- interactions:

  When model is set to `TRUE`, a vector of interaction effects to
  calculate. Will be passed to
  [`add_model()`](https://strohne.github.io/volker/reference/add_model.md).

- ...:

  Parameters passed to the
  [plot_metrics](https://strohne.github.io/volker/reference/plot_metrics.md)
  and
  [tab_metrics](https://strohne.github.io/volker/reference/tab_metrics.md)
  and
  [effect_metrics](https://strohne.github.io/volker/reference/effect_metrics.md)
  functions.

- index:

  When the cols contain items on a metric scale (as determined by
  [get_direction](https://strohne.github.io/volker/reference/get_direction.md)),
  an index will be calculated using the 'psych' package. Set to `FALSE`
  to suppress index generation.

- factors:

  The number of factors to calculate. Set to `FALSE` to suppress factor
  analysis. Set to `TRUE` to output a scree plot and automatically
  choose the number of factors. When the cols contain items on a metric
  scale (as determined by
  [get_direction](https://strohne.github.io/volker/reference/get_direction.md)),
  factors will be calculated using the 'psych' package. See
  [add_factors](https://strohne.github.io/volker/reference/add_factors.md).

- clusters:

  The number of clusters to calculate. Cluster are determined using
  kmeans after scaling the items. Set to `FALSE` to suppress cluster
  analysis. Set to `TRUE` to output a scree plot and automatically
  choose the number of clusters based on the elbow criterion. See
  [add_clusters](https://strohne.github.io/volker/reference/add_clusters.md).

- model:

  Set to `TRUE` for multivariable models if you don't provide your model
  as a formula. The dependent variable must be provided in the first
  parameter (cols). Independent categorical variables are provided in
  the second parameter (cross), which supports tidy column selections or
  vectors of multiple columns. Independent metric variables are provided
  in the metric parameter as tidy column selections or vectors of
  multiple columns. Interaction terms are provided in the interactions
  parameter and passed to the model functions. You can get diagnostic
  plots by setting the diagnostics parameter to TRUE. See
  [model_metrics_tab](https://strohne.github.io/volker/reference/model_metrics_tab.md),
  [model_metrics_plot](https://strohne.github.io/volker/reference/model_metrics_plot.md)
  and
  [add_model](https://strohne.github.io/volker/reference/add_model.md)
  for further options. Instead of providing the model variables as cols,
  cross, metric and interactions parameters, you can pass a model
  formula compatile with
  `stats::`[`lm`](https://rdrr.io/r/stats/lm.html) to the cols
  parameter,

- effect:

  Whether to report statistical tests and effect sizes. See
  [effect_counts](https://strohne.github.io/volker/reference/effect_counts.md)
  for further parameters.

- title:

  A character providing the heading or TRUE (default) to output a
  heading. Classes for tabset pills will be added.

- close:

  Whether to close the last tab (default value TRUE) or to keep it open.
  Keep it open to add further custom tabs by adding headers on the fifth
  level in Markdown (e.g. \##### Method).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

## Value

A volker report object.

## Details

For item batteries, an index is calculated and reported. When used in
combination with the Markdown-template "html_report", the different
parts of the report are grouped under a tabsheet selector.

**\[experimental\]**

## Examples

``` r
library(volker)
data <- volker::chatgpt

report_metrics(data, sd_age)

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
