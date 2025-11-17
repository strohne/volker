# Prepare data for calculation

Clean data, check column selection, remove cases with missing values

## Usage

``` r
data_prepare(
  data,
  cols,
  cross,
  cols.categorical,
  cols.numeric,
  cols.reverse,
  clean = TRUE
)
```

## Arguments

- data:

  Data frame to be prepared.

- cols:

  The first column selection.

- cross:

  The second column selection.

- cols.categorical:

  A tidy selection of columns to be checked for categorical values.

- cols.numeric:

  A tidy selection of columns to be converted to numeric values.

- cols.reverse:

  A tidy selection of columns with reversed codings.

- clean:

  Whether to clean data using
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

## Value

Prepared data frame.

## Examples

``` r
data <- volker::chatgpt
data_prepare(data, sd_age, sd_gender)
#> # A tibble: 101 × 22
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
#> # ℹ 91 more rows
#> # ℹ 17 more variables: cg_adoption_advantage_03 <dbl>,
#> #   cg_adoption_advantage_04 <dbl>, cg_adoption_fearofuse_01 <dbl>,
#> #   cg_adoption_fearofuse_02 <dbl>, cg_adoption_fearofuse_03 <dbl>,
#> #   cg_adoption_fearofuse_04 <dbl>, cg_adoption_social_01 <dbl>,
#> #   cg_adoption_social_02 <dbl>, cg_adoption_social_03 <dbl>,
#> #   cg_adoption_social_04 <dbl>, adopter <fct>, sd_age <dbl>, …
```
