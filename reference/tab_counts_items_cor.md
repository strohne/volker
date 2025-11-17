# Compare the values in multiple items by a metric column that will be split into groups

Compare the values in multiple items by a metric column that will be
split into groups

## Usage

``` r
tab_counts_items_cor(
  data,
  cols,
  cross,
  category = NULL,
  split = NULL,
  percent = TRUE,
  values = c("n", "p"),
  title = TRUE,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble containing item measures.

- cols:

  Tidyselect item variables (e.g. starts_with...).

- cross:

  A metric column that will be split into groups at the median value.

- category:

  Summarizing multiple items (the cols parameter) by group requires a
  focus category. By default, for logical column types, only TRUE values
  are counted. For other column types, the first category is counted.
  Accepts both character and numeric values to override default counting
  behavior.

- split:

  Not implemented yet.

- percent:

  Proportions are formatted as percent by default. Set to FALSE to get
  bare proportions.

- values:

  The values to output: n (frequency) or p (percentage) or both (the
  default).

- title:

  If TRUE (default) shows a plot title derived from the column labels.
  Disable the title with FALSE or provide a custom title as character
  value.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [plot_counts](https://strohne.github.io/volker/reference/plot_counts.md).

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt
tab_counts_items_cor(
  data, starts_with("cg_adoption_"), sd_age,
  category=c("agree", "strongly agree")
)
#> 
#> 
#> |Expectations                             |    total|  Low Age| High Age|
#> |:----------------------------------------|--------:|--------:|--------:|
#> |ChatGPT has clear advantages compared... | 51% (49)| 58% (26)| 44% (23)|
#> |Using ChatGPT brings financial benefits. | 27% (26)| 36% (16)| 19% (10)|
#> |Using ChatGPT is advantageous in many... | 63% (61)| 69% (31)| 58% (30)|
#> |Compared to other systems, using Chat... | 54% (52)| 58% (26)| 50% (26)|
#> |Much can go wrong when using ChatGPT.    | 35% (34)| 33% (15)| 37% (19)|
#> |There are legal issues with using Cha... | 30% (29)| 24% (11)| 35% (18)|
#> |The security of user data is not guar... | 33% (32)| 42% (19)| 25% (13)|
#> |Using ChatGPT could bring personal di... | 25% (24)| 29% (13)| 21% (11)|
#> |In my environment, using ChatGPT is s... | 21% (20)| 31% (14)|  12% (6)|
#> |Almost everyone in my environment use... | 16% (16)|  20% (9)|  13% (7)|
#> |Not using ChatGPT is considered being... | 12% (12)|  18% (8)|   8% (4)|
#> |Using ChatGPT brings me recognition f... | 20% (19)| 24% (11)|  15% (8)|
#> 
#> n=97. Frequencies based on values: agree, strongly agree. Age split at median 38. 4 missing case(s) omitted.
#> 
```
