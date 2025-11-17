# Test homogeneity of category shares for multiple items

Performs a goodness-of-fit test and calculates the Gini coefficient for
each item. The goodness-of-fit-test is calculated using
`stats::`[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html).

## Usage

``` r
effect_counts_items(
  data,
  cols,
  adjust = "fdr",
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

A volker tibble with the following statistical measures:

- **Gini coefficient**: Gini coefficient, measuring inequality.

- **n**: Number of cases the calculation is based on.

- **Chi-squared**: Chi-Squared test statistic.

- **p**: p-value for the statistical test.

- **stars**: Significance stars based on p-value (\*, \*\*, \*\*\*).

## Examples

``` r
library(volker)
data <- volker::chatgpt

effect_counts_items(data, starts_with("cg_adoption_adv"))
#> 
#> 
#> |Expectations                             | Gini coefficient|  n| Chi-squared|     p| stars|
#> |:----------------------------------------|----------------:|--:|-----------:|-----:|-----:|
#> |ChatGPT has clear advantages compared... |             0.36| 99|       43.47| 0.000|   ***|
#> |Using ChatGPT brings financial benefits. |             0.19| 99|       14.28| 0.006|    **|
#> |Using ChatGPT is advantageous in many... |             0.36| 99|       47.01| 0.000|   ***|
#> |Compared to other systems, using Chat... |             0.40| 99|       53.68| 0.000|   ***|
#> 
#> 2 missing case(s) omitted. Adjusted significance p values with fdr method.
#> 
```
