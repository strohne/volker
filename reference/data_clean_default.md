# Prepare data originating from SoSci Survey or SPSS

Preparation steps:

- Remove the avector class from all columns (comes from SoSci and
  prevents combining vectors)

- Recode residual factor values to NA (e.g.
  "[NA](https://rdrr.io/r/base/NA.html) nicht beantwortet")

- Recode residual numeric values to NA (e.g. -9)

## Usage

``` r
data_clean_default(data, remove.na.levels = TRUE, remove.na.numbers = TRUE)
```

## Arguments

- data:

  Data frame

- remove.na.levels:

  Remove residual values from factor columns. Either a character vector
  with residual values or TRUE to use defaults in
  [VLKR_NA_LEVELS](https://strohne.github.io/volker/reference/VLKR_NA_LEVELS.md).
  You can also define or disable residual levels by setting the global
  option vlkr.na.levels (e.g.
  `options(vlkr.na.levels=c("Not answered"))` or to disable
  `options(vlkr.na.levels=FALSE)`).

- remove.na.numbers:

  Remove residual values from numeric columns. Either a numeric vector
  with residual values or TRUE to use defaults in
  [VLKR_NA_NUMBERS](https://strohne.github.io/volker/reference/VLKR_NA_NUMBERS.md).
  You can also define or disable residual values by setting the global
  option vlkr.na.numbers (e.g. `options(vlkr.na.numbers=c(-2,-9))` or to
  disable `options(vlkr.na.numbers=FALSE)`).

## Value

Data frame with vlkr_df class (the class is used to prevent double
preparation).

## Details

The tibble remembers whether it was already prepared and the operations
are only performed once in the first call.

## Examples

``` r
ds <- volker::chatgpt
ds <- data_clean_default(ds)
```
