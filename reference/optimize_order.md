# Helper function: optimize the ordering of items for profile plots

Determines an ordering of the `item` factor levels so that grouped line
charts (e.g. cluster profile plots) read clearly, with as little line
"jumping" / crossing as possible. The returned vector can be passed to
[`.plot_lines()`](https://strohne.github.io/volker/reference/dot-plot_lines.md)
(or used directly to relevel the `item` factor).

## Usage

``` r
optimize_order(data, method = TRUE)
```

## Arguments

- data:

  A dataframe with the columns `item`, `value`, and `.cross`.

- method:

  The ordering method. Either `TRUE` to automatically select a method
  (`"olo"` if seriation is installed, otherwise `"min"`), or one of the
  character values `"max"`, `"min"`, `"spread"`, `"gw"`, or `"olo"`.
  Defaults to `TRUE`.

## Value

A vector of `item` values in the optimized order.

## Details

The following methods are supported:

- `TRUE`:

  Automatically select a method (default). Uses `"olo"` if the seriation
  package is available, otherwise falls back to `"min"`.

- `"max"`:

  Order items by descending grand mean of `value` (default). Produces a
  monotonic ladder.

- `"min"`:

  Order items by ascending grand mean of `value`.

- `"spread"`:

  Order items by the range (`max - min`) of `value` across groups.
  Highlights the items that discriminate the groups most.

- `"gw"`, `"olo"`:

  Use seriation to minimise crossings across all groups simultaneously.
  `"gw"` = Gruvaeus–Wainer, `"olo"` = optimal leaf ordering. Requires
  the seriation package.
