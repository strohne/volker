# A skimmer for metric variables

Inspired by the skimr package. Computes descriptive statistics (min,
quartiles, mean, sd, CI, items, alpha) for selected numeric variables,
respecting dplyr groupings and preserving attributes.

## Usage

``` r
skim_metrics(data, cols = tidyselect::where(is.numeric))
```

## Arguments

- data:

  A data frame or tibble (may be grouped with dplyr::group_by()).

- cols:

  Columns to summarise, specified using tidyselect (e.g.,
  where(is.numeric)).

## Value

A tibble with summary statistics per variable (and per group if
grouped).
