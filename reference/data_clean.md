# Prepare dataframe for the analysis

Depending on the selected cleaning plan, for example, recodes residual
values to NA.

## Usage

``` r
data_clean(data, plan = "default", ...)
```

## Arguments

- data:

  Data frame.

- plan:

  The cleaning plan. By now, only "default" is supported. See
  [data_clean_default](https://strohne.github.io/volker/reference/data_clean_default.md).

- ...:

  Other parameters passed to the appropriate cleaning function.

## Value

Cleaned data frame with vlkr_df class.

## Details

The tibble remembers whether it was already cleaned and the cleaning
plan is only applyed once in the first call.

## Examples

``` r
ds <- volker::chatgpt
ds <- data_clean(ds)
```
