# Remove NA numbers

Remove NA numbers

## Usage

``` r
data_rm_na_numbers(
  data,
  na.numbers = TRUE,
  check.labels = TRUE,
  default = VLKR_NA_NUMBERS
)
```

## Arguments

- data:

  Data frame

- na.numbers:

  Either a numeric vector with residual values or TRUE to use defaults
  in
  [VLKR_NA_NUMBERS](https://strohne.github.io/volker/reference/VLKR_NA_NUMBERS.md).
  You can also define residual values by setting the global option
  vlkr.na.numbers (e.g. `options(vlkr.na.numbers=c(-9))`).

- check.labels:

  Whether to only remove NA numbers that are listed in the attributes of
  a column.

- default:

  The default na numbers, if not explicitly provided by na.numbers or
  the options.

## Value

Data frame
