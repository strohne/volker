# Remove cases with only FALSE values in logical columns

Remove cases with only FALSE values in logical columns

## Usage

``` r
data_rm_empty(data, cols)
```

## Arguments

- data:

  Data frame.

- cols:

  A tidy column selection.

## Value

Data frame without empty rows. Removal information is added to the
misings attribute.
