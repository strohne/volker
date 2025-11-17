# Set an attribute value on selected columns of a data frame

Set an attribute value on selected columns of a data frame

## Usage

``` r
.attr_setcolumn(x, cols, attr_name, attr_value)
```

## Arguments

- x:

  A data frame containing the columns to modify.

- cols:

  A tidyselect expression specifying which columns to modify (e.g.,
  `c(var1, var2)` or `starts_with("score")`).

- attr_name:

  A character string giving the name of the attribute to set.

- attr_value:

  The value to assign to the attribute for all selected columns.

## Value

The data frame with the modified column attributes.
