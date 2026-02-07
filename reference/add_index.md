# Calculate the mean value of multiple items

**\[experimental\]**

## Usage

``` r
add_index(data, cols, newcol = NULL, cols.reverse, clean = TRUE)
```

## Arguments

- data:

  A dataframe.

- cols:

  A tidy selection of item columns.

- newcol:

  Name of the index as a character value. Set to NULL (default) to
  automatically build a name from the common column prefix, prefixed
  with "idx\_".

- cols.reverse:

  A tidy selection of columns with reversed codings.

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

## Value

The input tibble with an additional column that contains the index
values. The column contains the result of the alpha calculation in the
attribute named "psych.alpha".

## Examples

``` r
ds <- volker::chatgpt
volker::add_index(ds, starts_with("cg_adoption"))
#> Warning: response.frequency has been deprecated and replaced with responseFrequecy.  Please fix your call
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
