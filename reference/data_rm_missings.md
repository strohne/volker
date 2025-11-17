# Remove missings and output a message

Remove missings and output a message

## Usage

``` r
data_rm_missings(data, cols, force = FALSE)
```

## Arguments

- data:

  Data frame.

- cols:

  A tidy column selection.

- force:

  By default, cases with missings are only removed when the vlkr.na.omit
  option is TRUE. Set force to TRUE to always remove such cases.

## Value

Data frame.
