# Generate table and plot for agreement analysis

Generate table and plot for agreement analysis

## Usage

``` r
.report_agr(data, cols, coders, ids, method = "reliability", clean = TRUE, ...)
```

## Arguments

- data:

  A data frame.

- cols:

  Columns with codings. A tidy column selection, e.g. a single column
  (without quotes) or multiple columns selected by methods such as
  starts_with().

- coders:

  The coders or classification methods.

- ids:

  Column with case IDs.

- method:

  One of "reliability" or "classification".

## Value

A list containing a table and a plot volker report chunk.
