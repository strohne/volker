# Add cluster number to a data frame

Clustering is either performed using
`stats::`[`kmeans`](https://rdrr.io/r/stats/kmeans.html) (method =
"kmeans") on scaled numerical variables or using
`cluster::`[`pam`](https://rdrr.io/pkg/cluster/man/pam.html) on a Gower
dissimilarity matrix computed by
`cluster::`[`daisy`](https://rdrr.io/pkg/cluster/man/daisy.html) (method
= "pam").

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
  based on the elbow criterion.

- method:

  The method as character value. One of "kmeans" (default) or "pam". For
  "kmeans" all items will be converted to numerical values and scaled
  using `base::`[`scale`](https://rdrr.io/r/base/scale.html). Euclidean
  distance is used. For "pam", all items will be converted to
  categorical values by
  [data_cat](https://strohne.github.io/volker/reference/data_cat.md). A
  Gower dissimilarity matrix is used. Note that logical values are
  treated as asymmetrical, i.e. `FALSE` does not have a meaning, when
  computing the gower metric. Therefore cases with only `FALSE` values
  (no annotations, no codes) are removed.

- labels:

  Whether to get the label of the cluster column from the common prefix
  of item column labels.

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

## Value

The input tibble with an additional cluster column (factor, prefixed
"cls\_"). The fit result is stored in the attribute stats.cluster.fit,
the item names in stats.cluster.items, the scree-plot or silhouette-plot
data in stats.cluster.diag and the method in stats.cluster.method.

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
volker::add_clusters(ds, starts_with("cg_adoption"), k = 3, method = "pam")
#> # A tibble: 97 × 24
#>     case use_private use_work cg_adoption_advantage_01 cg_adoption_advantage_02
#>    <dbl>       <dbl>    <dbl> <fct>                    <fct>                   
#>  1   170           4        4 3                        4                       
#>  2   183           1        1 4                        3                       
#>  3   195           2        4 5                        5                       
#>  4   212           5        5 4                        4                       
#>  5   222           2        3 3                        2                       
#>  6   236           3        1 3                        2                       
#>  7   255           3        1 3                        1                       
#>  8   297           3        4 4                        3                       
#>  9   309           3        3 3                        4                       
#> 10   325           2        1 4                        1                       
#> # ℹ 87 more rows
#> # ℹ 19 more variables: cg_adoption_advantage_03 <fct>,
#> #   cg_adoption_advantage_04 <fct>, cg_adoption_fearofuse_01 <fct>,
#> #   cg_adoption_fearofuse_02 <fct>, cg_adoption_fearofuse_03 <fct>,
#> #   cg_adoption_fearofuse_04 <fct>, cg_adoption_social_01 <fct>,
#> #   cg_adoption_social_02 <fct>, cg_adoption_social_03 <fct>,
#> #   cg_adoption_social_04 <fct>, adopter <fct>, sd_age <dbl>, …
```
