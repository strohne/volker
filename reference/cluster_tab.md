# Get tables for clustering result

Kmeans clustering is performed using
[add_clusters](https://strohne.github.io/volker/reference/add_clusters.md).

**\[experimental\]**

## Usage

``` r
cluster_tab(
  data,
  cols,
  newcol = NULL,
  k = NULL,
  method = "kmeans",
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
  values as a factor. If the column already contains a cluster result
  from
  [add_clusters](https://strohne.github.io/volker/reference/add_clusters.md),
  it is used, and other parameters are ignored. If no cluster result
  exists, it is calculated with
  [add_clusters](https://strohne.github.io/volker/reference/add_clusters.md).

- newcol:

  Name of the new cluster column as a character vector. Set to NULL
  (default) to automatically build a name from the common column prefix,
  prefixed with "cls\_".

- k:

  Number of clusters to calculate. Set to NULL to output a scree plot
  for up to 10 clusters and automatically choose the number of clusters
  based on the elbow criterion. The within-sums of squares for the scree
  plot are calculated by
  `stats::`[`kmeans`](https://rdrr.io/r/stats/kmeans.html).

- method:

  The method as character value. Currently, only kmeans is supported.
  All items are scaled before performing the cluster analysis using
  `base::`[`scale`](https://rdrr.io/r/base/scale.html).

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

A volker list with with three volker tabs: cluster centers, cluster
counts, and clustering diagnostics.

## Examples

``` r
library(volker)
data <- volker::chatgpt

cluster_tab(data, starts_with("cg_adoption"), k = 2)
#> 
#> 
#> |Expectations                             |     total| Cluster 1| Cluster 2|
#> |:----------------------------------------|---------:|---------:|---------:|
#> |ChatGPT has clear advantages compared... | 3.4 (1.0)| 3.8 (0.9)| 3.2 (1.0)|
#> |Using ChatGPT brings financial benefits. | 2.7 (1.2)| 3.5 (0.9)| 2.2 (1.1)|
#> |Using ChatGPT is advantageous in many... | 3.6 (1.1)| 4.0 (0.8)| 3.3 (1.2)|
#> |Compared to other systems, using Chat... | 3.5 (1.0)| 4.0 (0.8)| 3.2 (1.0)|
#> |Much can go wrong when using ChatGPT.    | 3.1 (1.1)| 3.1 (1.1)| 3.1 (1.1)|
#> |There are legal issues with using Cha... | 3.1 (1.2)| 3.5 (1.0)| 2.8 (1.2)|
#> |The security of user data is not guar... | 3.2 (1.0)| 3.5 (1.0)| 3.0 (1.0)|
#> |Using ChatGPT could bring personal di... | 2.7 (1.1)| 3.1 (1.2)| 2.5 (1.0)|
#> |In my environment, using ChatGPT is s... | 2.5 (1.1)| 3.5 (0.9)| 1.9 (0.8)|
#> |Almost everyone in my environment use... | 2.4 (1.2)| 3.4 (1.0)| 1.8 (0.8)|
#> |Not using ChatGPT is considered being... | 2.0 (1.2)| 2.9 (1.2)| 1.4 (0.6)|
#> |Using ChatGPT brings me recognition f... | 2.3 (1.2)| 3.4 (1.1)| 1.7 (0.8)|
#> |n                                        |        97|        36|        61|
#> 
#> n=97. 4 missing case(s) omitted.
#> 
#> 
#> 
#> |Cluster   |  n|    p|
#> |:---------|--:|----:|
#> |Cluster 1 | 36|  37%|
#> |Cluster 2 | 61|  63%|
#> |total     | 97| 100%|
#> 
#> n=97.
#> 
#> 
#> 
#> |Statistic                      |  Value|
#> |:------------------------------|------:|
#> |Within-Cluster Sum of Squares  | 907.02|
#> |Between-Cluster Sum of Squares | 244.98|
```
