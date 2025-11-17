# Remove NA levels

Remove NA levels

## Usage

``` r
data_rm_na_levels(data, na.levels = TRUE, default = VLKR_NA_LEVELS)
```

## Arguments

- data:

  Data frame

- na.levels:

  Residual values to remove from factor columns. Either a character
  vector with residual values or TRUE to use defaults in
  [VLKR_NA_LEVELS](https://strohne.github.io/volker/reference/VLKR_NA_LEVELS.md).
  You can define default residual levels by setting the global option
  vlkr.na.levels (e.g. `options(vlkr.na.levels=c("Not answered"))`).

- default:

  The default na levels, if not explicitly provided by na.levels or the
  options.

## Value

Data frame
