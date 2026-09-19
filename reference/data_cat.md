# Convert numeric values to factors, characters or logical values while preserving attributes

Convert numeric values to factors, characters or logical values while
preserving attributes

## Usage

``` r
data_cat(data, cols, type)
```

## Arguments

- data:

  A data frame containing the items to be converted.

- cols:

  A tidy selection of columns to convert.

- type:

  The target type, one of `factor`, `character`, or `logical`. If the
  type is missing, only numeric values will be converted to character.

## Value

A data frame with the converted values
