# Calculate agreement coefficients for multiple items

Calculate agreement coefficients for multiple items

## Usage

``` r
.agree_items(
  data,
  cols,
  coders,
  ids = NULL,
  category = NULL,
  method = "reliability",
  labels = TRUE
)
```

## Arguments

- data:

  A tibble.

- cols:

  The columns holding codings.

- coders:

  The column holding coders.

- ids:

  The column holding case identifiers.

- category:

  If no category is provided, macro statistics are returned (along with
  the number of categories in the output). Provide a category to get the
  statistics for this category only. If values are boolean (TRUE /
  FALSE) and no category is provided, the category is always assumed to
  be "TRUE".

- method:

  The output metrics, one of reliability (reliability scores) or
  classification (accuracy, precision, recall, f1).

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

## Value

A tibble with agreement coefficients.
