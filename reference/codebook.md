# Get variable and value labels from a data set

Variable labels are extracted from their comment or label attribute.
Variable values are extracted from factor levels, the labels attribute,
numeric or boolean attributes.

## Usage

``` r
codebook(data, cols, values = TRUE)
```

## Arguments

- data:

  A tibble.

- cols:

  A tidy variable selections to filter specific columns.

- values:

  Whether to output values (TRUE) or only items (FALSE)

## Value

A tibble with the columns:

- item_name: The column name.

- item_group: First part of the column name, up to an underscore.

- item_class: The last class value of an item (e.g. numeric, factor).

- item_label: The comment attribute of the column.

- value_name: In case a column has numeric attributes, the attribute
  names.

- value_label: In case a column has numeric attributes or
  T/F-attributes, the attribute values. In case a column has a levels
  attribute, the levels.

## Details

**\[experimental\]**

## Examples

``` r
volker::codebook(volker::chatgpt)
#> # A tibble: 97 × 6
#>    item_name     item_group item_class item_label         value_name value_label
#>    <chr>         <chr>      <chr>      <chr>              <chr>      <chr>      
#>  1 case          case       numeric    case               NA         NA         
#>  2 sd_age        sd         numeric    Age                NA         NA         
#>  3 cg_activities cg         character  Activities with C… NA         NA         
#>  4 cg_act_write  cg         character  cg_act_write       NA         NA         
#>  5 cg_act_test   cg         character  cg_act_test        NA         NA         
#>  6 cg_act_search cg         character  cg_act_search      NA         NA         
#>  7 adopter       adopter    factor     Innovator type     I try new… I try new …
#>  8 adopter       adopter    factor     Innovator type     I try new… I try new …
#>  9 adopter       adopter    factor     Innovator type     I wait un… I wait unt…
#> 10 adopter       adopter    factor     Innovator type     I only us… I only use…
#> # ℹ 87 more rows
```
