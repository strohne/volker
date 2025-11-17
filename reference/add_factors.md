# Add PCA columns along with summary statistics (KMO and Bartlett test) to a data frame

PCA is performed using
`psych::`[`pca`](https://rdrr.io/pkg/psych/man/principal.html) usind
varimax rotation. Bartlett's test for sphericity is calculated with
`psych::`[`cortest.bartlett`](https://rdrr.io/pkg/psych/man/cortest.bartlett.html).
The Kaiser-Meyer-Olkin (KMO) measure is computed using
`psych::`[`KMO`](https://rdrr.io/pkg/psych/man/KMO.html).

**\[experimental\]**

## Usage

``` r
add_factors(data, cols, newcols = NULL, k = 2, method = "pca", clean = TRUE)
```

## Arguments

- data:

  A dataframe.

- cols:

  A tidy selection of item columns.

- newcols:

  Names of the factor columns as a character vector. Must be the same
  length as k or NULL. Set to NULL (default) to automatically build a
  name from the common column prefix, prefixed with "fct\_", postfixed
  with the factor number.

- k:

  Number of factors to calculate. Set to NULL to calculate eigenvalues
  for all components up to the number of items and automatically
  choose k. Eigenvalues and the decision on k are calculated by
  `psych::`[`fa.parallel`](https://rdrr.io/pkg/psych/man/fa.parallel.html).

- method:

  The method as character value. Currently, only pca is supported.

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

## Value

The input tibble with additional columns containing factor values. The
new columns are prefixed with "fct\_". The first new column contains the
fit result in the attribute psych.pca.fit. The names of the items used
for factor analysis are stored in the attribute psych.pca.items. The
summary diagnostics (Bartlett test and KMO) are stored in the attribute
psych.kmo.bartlett.

## Examples

``` r
library(volker)
ds <- volker::chatgpt

volker::add_factors(ds, starts_with("cg_adoption"))
#> # A tibble: 97 × 24
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
#> # ℹ 19 more variables: cg_adoption_advantage_03 <dbl>,
#> #   cg_adoption_advantage_04 <dbl>, cg_adoption_fearofuse_01 <dbl>,
#> #   cg_adoption_fearofuse_02 <dbl>, cg_adoption_fearofuse_03 <dbl>,
#> #   cg_adoption_fearofuse_04 <dbl>, cg_adoption_social_01 <dbl>,
#> #   cg_adoption_social_02 <dbl>, cg_adoption_social_03 <dbl>,
#> #   cg_adoption_social_04 <dbl>, adopter <fct>, sd_age <dbl>, …
```
