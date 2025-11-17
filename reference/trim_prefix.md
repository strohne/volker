# Remove a prefix from a character vector or a factor

If the resulting character values would be empty, the prefix is
returned. At the end, all items in the vector are trimmed using
[trim_label](https://strohne.github.io/volker/reference/trim_label.md).

## Usage

``` r
trim_prefix(x, prefix = TRUE)
```

## Arguments

- x:

  A character or factor vector.

- prefix:

  The prefix. Set to TRUE to first extract the prefix.

## Value

The trimmed character or factor vector.

## Details

If x is a factor, the order of factor levels is retained.
