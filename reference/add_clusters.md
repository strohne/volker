# Add cluster number to a data frame

Clustering is performed using
`stats::`[`kmeans`](https://rdrr.io/r/stats/kmeans.html).

**\[experimental\]**

## Usage

``` r
add_clusters(
  data,
  cols,
  newcol = NULL,
  k = 2,
  method = "kmeans",
  labels = TRUE,
  clean = TRUE
)
```

## Arguments

- data:

  A dataframe.

- cols:

  A tidy selection of item columns.

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

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

## Value

The input tibble with additional column containing cluster values as a
factor. The new column is prefixed with "cls\_". The new column contains
the fit result in the attribute stats.kmeans.fit. The names of the items
used for clustering are stored in the attribute stats.kmeans.items. The
clustering diagnostics (Within-Cluster and Between-Cluster Sum of
Squares) are stored in the attribute stats.kmeans.wss.

## Examples

``` r
library(volker)
ds <- volker::chatgpt

volker::add_clusters(ds, starts_with("cg_adoption"), k = 3)
#> # A tibble: 97 × 23
#>     case use_private use_work cg_adoption_advantage_01 cg_adoption_advantage_02
#>    <dbl>       <dbl>    <dbl>                    <dbl>                    <dbl>
#>  1   170           4        4                        3                        4
#>  2   183           1        1                        4                        3
#>  3   195           2        4                        5                        5
#>  4   212           5        5                        4                        4
#>  5   222           2        3                        3                        2
#>  6   236           3        1                        3                        2
#>  7   255           3        1                        3                        1
#>  8   297           3        4                        4                        3
#>  9   309           3        3                        3                        4
#> 10   325           2        1                        4                        1
#> # ℹ 87 more rows
#> # ℹ 18 more variables: cg_adoption_advantage_03 <dbl>,
#> #   cg_adoption_advantage_04 <dbl>, cg_adoption_fearofuse_01 <dbl>,
#> #   cg_adoption_fearofuse_02 <dbl>, cg_adoption_fearofuse_03 <dbl>,
#> #   cg_adoption_fearofuse_04 <dbl>, cg_adoption_social_01 <dbl>,
#> #   cg_adoption_social_02 <dbl>, cg_adoption_social_03 <dbl>,
#> #   cg_adoption_social_04 <dbl>, adopter <fct>, sd_age <dbl>, …
```
