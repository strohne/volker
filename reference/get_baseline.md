# Get a formatted baseline from attributes of an object.

The following attributes are considered:

- cases: Number of cases.

- missing: Removed zero, negative, and missing cases.

- focus: Focus category.

- auto: The k value of cluster methods.

- reversed: A list of reversed items.

- split: Items that were split at the median.

- adjust: The adjustment method for p values.

## Usage

``` r
get_baseline(obj, ignore = c())
```

## Arguments

- obj:

  An object with supported attributes.

- ignore:

  Characer vector of attributes to ignore.

## Value

A formatted message or NULL if none of the supported attributes is
present.
