# Replace item value names in a column by their labels

Replace item value names in a column by their labels

## Usage

``` r
labs_replace(
  data,
  col,
  codes,
  col_from = "value_name",
  col_to = "value_label",
  na.missing = FALSE
)
```

## Arguments

- data:

  A tibble.

- col:

  The column holding item values.

- codes:

  The codebook to use: A tibble with the columns value_name and
  value_label. Can be created by the
  [codebook](https://strohne.github.io/volker/reference/codebook.md)
  function, e.g. by calling `codes <- codebook(data, myitemcolumn)`.

- col_from:

  The tidyselect column with source values, defaults to value_name. If
  the column is not found in the codebook, the first column is used.

- col_to:

  The tidyselect column with target values, defaults to value_label. If
  the column is not found in the codebook, the second column is used

- na.missing:

  By default, the column is converted to a factor with levels combined
  from the codebook and the data. Set na.missing to TRUE to set all
  levels not found in the codes to NA.

## Value

Tibble with new labels.
