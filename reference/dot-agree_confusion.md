# Generate confusion matrix

Generate confusion matrix

## Usage

``` r
.agree_confusion(
  data,
  cols,
  coders,
  ids = NULL,
  category = NULL,
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

  Focus category or null.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

## Value

A tibble representing a confusion matrix.
