# Add vlkr_tbl class

Additionally, removes the skim_df class if present.

## Usage

``` r
.to_vlkr_tab(data, digits = NULL, caption = NULL, baseline = NULL)
```

## Arguments

- data:

  A tibble.

- digits:

  Set the plot digits. If NULL (default), no digits are set.

- caption:

  The caption printed above the table.

- baseline:

  A base line printed below the table.

## Value

A volker tibble.
