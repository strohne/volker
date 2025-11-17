# Effect size and test for comparing multiple variables by a grouping variable

Effect size and test for comparing multiple variables by a grouping
variable

## Usage

``` r
effect_counts_items_grouped(
  data,
  cols,
  cross,
  method = "cramer",
  adjust = "fdr",
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble containing item measures and grouping variable.

- cols:

  Tidyselect item variables (e.g. starts_with...).

- cross:

  The column holding groups to compare.

- method:

  The output metrics, currently only `cramer` is supported.

- adjust:

  Performing multiple significance tests inflates the alpha error. Thus,
  p values need to be adjusted according to the number of tests. Set a
  method supported by
  `stats::`[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html), e.g.
  "fdr" (the default) or "bonferroni". Disable adjustment with FALSE.

- labels:

  If TRUE (default) extracts labels from the attributes, see
  [codebook](https://strohne.github.io/volker/reference/codebook.md).

- clean:

  Prepare data by
  [data_clean](https://strohne.github.io/volker/reference/data_clean.md).

- ...:

  Placeholder to allow calling the method with unused parameters from
  [effect_counts](https://strohne.github.io/volker/reference/effect_counts.md).

## Value

A volker tibble.

## Examples

``` r
library(volker)
data <- volker::chatgpt

effect_counts_items_grouped(
  data, starts_with("cg_adoption_adv"),  sd_gender
)
#> 
#> 
#> |Expectations: Correlation w...           | Cramer's V| Chi-squared|  n| df|     p| stars|
#> |:----------------------------------------|----------:|-----------:|--:|--:|-----:|-----:|
#> |ChatGPT has clear advantages compared... |       0.14|        3.76| 99|   | 0.851|      |
#> |Using ChatGPT brings financial benefits. |       0.16|        4.99| 99|   | 0.851|      |
#> |Using ChatGPT is advantageous in many... |       0.14|        3.78| 99|   | 0.851|      |
#> |Compared to other systems, using Chat... |       0.13|        3.44| 99|   | 0.851|      |
#> 
#> n=99. 2 missing case(s) omitted. Adjusted significance p values with fdr method.
#> 
```
