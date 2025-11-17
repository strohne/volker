# Compare the values in multiple items by a grouping column

Compare the values in multiple items by a grouping column

## Usage

``` r
tab_counts_items_grouped(
  data,
  cols,
  cross,
  category = NULL,
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

  The column holding groups to compare.

- category:

  Summarizing multiple items (the cols parameter) by group requires a
  focus category. By default, for logical column types, only TRUE values
  are counted. For other column types, the first category is counted.
  Accepts both character and numeric values to override default counting
  behavior.

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
tab_counts_items_grouped(
  data, starts_with("cg_adoption_"), adopter,
  category=c("agree", "strongly agree")
)
#> 
#> 
#> |Expectations                             |    total| I try new offers immediately| I try new offers rather qui...| I wait until offers establi...| I only use new offers when ...|
#> |:----------------------------------------|--------:|----------------------------:|------------------------------:|------------------------------:|------------------------------:|
#> |ChatGPT has clear advantages compared... | 51% (49)|                      53% (8)|                       51% (31)|                       50% (10)|                         0% (0)|
#> |Using ChatGPT brings financial benefits. | 27% (26)|                      53% (8)|                       26% (16)|                        10% (2)|                         0% (0)|
#> |Using ChatGPT is advantageous in many... | 63% (61)|                     73% (11)|                       66% (40)|                       50% (10)|                         0% (0)|
#> |Compared to other systems, using Chat... | 54% (52)|                     67% (10)|                       49% (30)|                       60% (12)|                         0% (0)|
#> |Much can go wrong when using ChatGPT.    | 35% (34)|                      33% (5)|                       38% (23)|                        30% (6)|                         0% (0)|
#> |There are legal issues with using Cha... | 30% (29)|                      47% (7)|                       31% (19)|                        15% (3)|                         0% (0)|
#> |The security of user data is not guar... | 33% (32)|                      60% (9)|                       30% (18)|                        25% (5)|                         0% (0)|
#> |Using ChatGPT could bring personal di... | 25% (24)|                      47% (7)|                       23% (14)|                        15% (3)|                         0% (0)|
#> |In my environment, using ChatGPT is s... | 21% (20)|                     67% (10)|                        13% (8)|                        10% (2)|                         0% (0)|
#> |Almost everyone in my environment use... | 16% (16)|                      60% (9)|                        10% (6)|                         5% (1)|                         0% (0)|
#> |Not using ChatGPT is considered being... | 12% (12)|                      53% (8)|                         5% (3)|                         5% (1)|                         0% (0)|
#> |Using ChatGPT brings me recognition f... | 20% (19)|                      53% (8)|                        13% (8)|                        15% (3)|                         0% (0)|
#> 
#> n=97. Frequencies based on values: agree, strongly agree. 4 missing case(s) omitted.
#> 
```
