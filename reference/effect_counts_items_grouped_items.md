# Effect size and test for comparing multiple variables by multiple grouping variables

Effect size and test for comparing multiple variables by multiple
grouping variables

## Usage

``` r
effect_counts_items_grouped_items(
  data,
  cols,
  cross,
  method = "cramer",
  adjust = "fdr",
  category = NULL,
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

  The columns holding groups to compare.

- method:

  The output metrics: cramer = Cramer's V, pmi = Pointwise Mutual
  Information, npmi = Normalized PMI.

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

effect_counts(
  data,
  starts_with("cg_adoption_adv"),
  starts_with("use_")
)
#> 
#> 
#> |Expectations                             |                   Usage| Cramer's V| Chi-squared|  n| df|     p| stars|
#> |:----------------------------------------|-----------------------:|----------:|-----------:|--:|--:|-----:|-----:|
#> |ChatGPT has clear advantages compared... |      in private context|       0.32|       41.27| 99|   | 0.006|    **|
#> |ChatGPT has clear advantages compared... | in professional context|       0.24|       23.46| 99|   | 0.114|      |
#> |Using ChatGPT brings financial benefits. |      in private context|       0.24|       22.48| 99|   | 0.135|      |
#> |Using ChatGPT brings financial benefits. | in professional context|       0.37|       53.99| 99|   | 0.004|    **|
#> |Using ChatGPT is advantageous in many... |      in private context|       0.25|       24.15| 99|   | 0.114|      |
#> |Using ChatGPT is advantageous in many... | in professional context|       0.30|       34.57| 99|   | 0.019|     *|
#> |Compared to other systems, using Chat... |      in private context|       0.30|       34.62| 99|   | 0.021|     *|
#> |Compared to other systems, using Chat... | in professional context|       0.20|       16.23| 99|   | 0.441|      |
#> 
#> n=99. 2 missing case(s) omitted. Adjusted significance p values with fdr method.
#> 
```
