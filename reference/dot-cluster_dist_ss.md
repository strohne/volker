# Sum-of-squares decomposition for a dissimilarity matrix

Uses the Huygens decomposition of total dispersion for an arbitrary
dissimilarity matrix, so that within- and between-cluster "sum of
squares" can be reported for distance-based methods such as PAM with
Gower distance.

## Usage

``` r
.cluster_dist_ss(dissim, clustering)
```

## Arguments

- dissim:

  A dist object.

- clustering:

  An integer vector with cluster assignments.

## Value

A list with tot.withinss, betweenss and totss.
