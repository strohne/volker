# Get plot for clustering result

Clustering is performed using
[add_clusters](https://strohne.github.io/volker/reference/add_clusters.md).

**\[experimental\]**

## Usage

``` r
cluster_plot(
  data,
  cols,
  newcol = NULL,
  k = NULL,
  method = NULL,
  type = "lines",
  reorder = TRUE,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- cols:

  A tidy selection of item columns or a single column with cluster
  values as a factor or logical. If the column already contains a
  cluster result from
  [add_clusters](https://strohne.github.io/volker/reference/add_clusters.md),
  it is used, and other parameters are ignored. If no cluster result
  exists, it is calculated with
  [add_clusters](https://strohne.github.io/volker/reference/add_clusters.md).

- newcol:

  Name of the new cluster column as a character vector. Set to NULL
  (default) to automatically build a name from the common column prefix,
  prefixed with "cls\_".

- k:

  Number of clusters to calculate. Set to NULL to automatically
  determine an optimal cluster number. For kmeans, outputs a scree plot
  based on within-sums of squares for up to 10 clusters. In this case,
  the number of clusters is automatically chosed based on the elbow
  criterion. For pam, outputs a silhoette plot for up to 10 clusters. In
  this case, the number of clusters is automatically chosen based on the
  maximum average silhouette.

- method:

  The method as a character value, one of `kmeans` or `pam`. See
  [add_clusters](https://strohne.github.io/volker/reference/add_clusters.md)
  for further details.

- type:

  The plot type, one of `"lines"` or `"heatmap"`.

- reorder:

  Reorder items to minimize line crossings, Either `TRUE` to
  automatically select a method (`"olo"` if seriation is installed,
  otherwise `"min"`), or one of the character values `"max"`, `"min"`,
  `"spread"`, `"gw"`, or `"olo"`. Defaults to `FALSE` which disables
  reordering.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [plot_metrics](https://strohne.github.io/volker/reference/plot_metrics.md).

## Value

A ggplot object.

## Examples

``` r
library(volker)
data <- volker::chatgpt

cluster_plot(data, starts_with("cg_adoption"), k = 2)

#> In the plot, 4 missing case(s) omitted.
```
