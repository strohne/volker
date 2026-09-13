# Fit PAM clustering with a kmeans-like return structure

Wraps `cluster::`[`pam`](https://rdrr.io/pkg/cluster/man/pam.html) on a
precomputed dissimilarity matrix and normalises the result to the
structure returned by
`stats::`[`kmeans`](https://rdrr.io/r/stats/kmeans.html), so it can be
used interchangeably in the clustering workflow. For k = 1 a trivial
one-cluster solution is built (PAM itself requires k \>= 2), which
allows the scree plot to start at k = 1.

Within- and between-cluster sum of squares are derived from the
dissimilarity matrix via
[.cluster_dist_ss](https://strohne.github.io/volker/reference/dot-cluster_dist_ss.md).

## Usage

``` r
.cluster_pam(dissim, k)
```

## Arguments

- dissim:

  A dist object (e.g. from
  `cluster::`[`daisy`](https://rdrr.io/pkg/cluster/man/daisy.html)).

- k:

  Number of clusters.

## Value

A list with elements cluster, size, tot.withinss, betweenss and totss,
and avg.silwidth. For k \> 1, returns also the raw pam object.
