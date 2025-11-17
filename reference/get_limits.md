# Get the numeric range from the labels

Gets the range of all values in the selected columns by the first
successful of the following methods:

## Usage

``` r
get_limits(data, cols, negative = TRUE)
```

## Arguments

- data:

  The labeled data frame.

- cols:

  A tidy variable selection.

- negative:

  Whether to include negative values.

## Value

A list or NULL.

## Details

- Inspect the limits column attribute.

- Lookup the value names in the codebook.

- Calculate the range from all values in the columns.
