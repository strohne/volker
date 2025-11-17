# Get tables with factor analysis results

PCA is performed using
[add_factors](https://strohne.github.io/volker/reference/add_factors.md).

**\[experimental\]**

## Usage

``` r
factor_tab(
  data,
  cols,
  newcols = NULL,
  k = 2,
  method = "pca",
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A dataframe.

- cols:

  A tidy selection of item columns.

              If the first column already contains a pca result from \link{add_factors},
              the result is used. Other parameters are ignored.

              If there is no pca result yet, it is calculated by \link{add_factors} first.

- newcols:

  Names of the new factor columns as a character vector. Must be the
  same length as k or NULL. Set to NULL (default) to automatically build
  a name from the common column prefix, prefixed with "fct\_", postfixed
  with the factor number.

- k:

  Number of factors to calculate. Set to NULL to report eigenvalues for
  all components up to the number of items and automatically choose k.
  Eigenvalues and the decision on k are calculated by
  `psych::`[`fa.parallel`](https://rdrr.io/pkg/psych/man/fa.parallel.html).

- method:

  The method as character value. Currently, only pca is supported.

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

A volker list with with three volker tabs: loadings, variances and
diagnostics.

## Examples

``` r
library(volker)
ds <- volker::chatgpt

volker::factor_tab(ds, starts_with("cg_adoption"), k = 3)
#> 
#> 
#> |Expectations                             | Component 1| Component 2| Component 3| communality|
#> |:----------------------------------------|-----------:|-----------:|-----------:|-----------:|
#> |ChatGPT has clear advantages compared... |         0.1|         0.9|         0.0|         0.8|
#> |Using ChatGPT brings financial benefits. |         0.5|         0.5|         0.3|         0.6|
#> |Using ChatGPT is advantageous in many... |         0.2|         0.8|         0.0|         0.7|
#> |Compared to other systems, using Chat... |         0.2|         0.8|         0.0|         0.7|
#> |Much can go wrong when using ChatGPT.    |        -0.1|        -0.2|         0.8|         0.7|
#> |There are legal issues with using Cha... |         0.2|         0.2|         0.6|         0.5|
#> |The security of user data is not guar... |         0.1|         0.1|         0.7|         0.6|
#> |Using ChatGPT could bring personal di... |         0.2|        -0.1|         0.7|         0.6|
#> |In my environment, using ChatGPT is s... |         0.9|         0.2|         0.0|         0.8|
#> |Almost everyone in my environment use... |         0.8|         0.2|         0.1|         0.7|
#> |Not using ChatGPT is considered being... |         0.7|         0.0|         0.3|         0.6|
#> |Using ChatGPT brings me recognition f... |         0.8|         0.2|         0.0|         0.6|
#> 
#> 4 missing case(s) omitted.
#> 
#> 
#> 
#> |Component   | Eigenvalue| Proportion of variance| Cumulative proportion of va...|
#> |:-----------|----------:|----------------------:|------------------------------:|
#> |Component 1 |        3.0|                    0.3|                            0.3|
#> |Component 2 |        2.5|                    0.2|                            0.5|
#> |Component 3 |        2.2|                    0.2|                            0.6|
#> 
#> 
#> |Test          |                Statistic|  value|
#> |:-------------|------------------------:|------:|
#> |KMO Test      |                    Cases|     97|
#> |KMO Test      |                Variables|     12|
#> |KMO Test      | Cases-to-Variables Ratio|   8.08|
#> |KMO Test      |              Overall MSA|   0.74|
#> |Bartlett Test |              Chi-squared| 463.54|
#> |Bartlett Test |                       df|     66|
#> |Bartlett Test |                        p|  0.000|
#> |Bartlett Test |                    stars|    ***|
```
