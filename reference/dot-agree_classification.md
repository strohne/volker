# Calculate classification performance indicators such as precision and recall.

Calculate classification performance indicators such as precision and
recall.

## Usage

``` r
.agree_classification(x, y, ids = NULL, category = NULL)
```

## Arguments

- x:

  Vactor with values.

- y:

  Vector with sources, the first value is taken as ground truth source.
  To change the ground truth, use a factor and set the first factor
  level to the appropriate value.

- ids:

  Vector of Case IDs.

- category:

  If no category is provided, macro statistics are returned (along with
  the number of categories in the output). Provide a category to get the
  statistics for this category only. If values are boolean (TRUE /
  FALSE) and no category is provided, the category is always assumed to
  be "TRUE".

## Value

A list with classification performance indicators.
