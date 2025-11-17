# Compare groups for each item by calculating F-statistics and effect sizes

The models are fitted using
`stats::`[`lm`](https://rdrr.io/r/stats/lm.html). ANOVA of type II is
computed for each fitted model using
`car::`[`Anova`](https://rdrr.io/pkg/car/man/Anova.html). Eta Squared is
calculated for each ANOVA result using
`effectsize::`[`eta_squared`](https://easystats.github.io/effectsize/reference/eta_squared.html).

## Usage

``` r
effect_metrics_items_grouped(
  data,
  cols,
  cross,
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

- cross:

  The column holding groups to compare.

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
  [effect_metrics](https://strohne.github.io/volker/reference/effect_metrics.md).

## Value

A volker tibble with the following statistical measures:

- **Eta-squared**: Effect size indicating the proportion of variance in
  the dependent variable explained by the predictor.

- **Eta**: Root of Eta-squared, a standardized effect size.

- **n**: Number of cases the calculation is based on.

- **F**: F-statistic from the linear model.

- **p**: p-value for the statistical test.

- **stars**: Significance stars based on p-value (\*, \*\*, \*\*\*).

## Examples

``` r
library(volker)
data <- volker::chatgpt

effect_metrics(data, starts_with("cg_adoption_"), adopter)
#> 
#> 
#> |Expectations                             | Eta-squared|  Eta|  n|     F|     p| stars|
#> |:----------------------------------------|-----------:|----:|--:|-----:|-----:|-----:|
#> |ChatGPT has clear advantages compared... |        0.03| 0.18| 97|  1.03| 0.719|      |
#> |Using ChatGPT brings financial benefits. |        0.08| 0.29| 97|  2.88| 0.026|     *|
#> |Using ChatGPT is advantageous in many... |        0.01| 0.09| 97|  0.23| 0.630|      |
#> |Compared to other systems, using Chat... |        0.03| 0.19| 97|  1.12| 0.206|      |
#> |Much can go wrong when using ChatGPT.    |        0.00| 0.07| 97|  0.16| 0.621|      |
#> |There are legal issues with using Cha... |        0.05| 0.21| 97|  1.47| 0.221|      |
#> |The security of user data is not guar... |        0.04| 0.21| 97|  1.37| 0.219|      |
#> |Using ChatGPT could bring personal di... |        0.04| 0.20| 97|  1.36| 0.219|      |
#> |In my environment, using ChatGPT is s... |        0.22| 0.47| 97|  8.96| 0.000|   ***|
#> |Almost everyone in my environment use... |        0.23| 0.48| 97|  9.05| 0.000|   ***|
#> |Not using ChatGPT is considered being... |        0.27| 0.52| 97| 11.63| 0.000|   ***|
#> |Using ChatGPT brings me recognition f... |        0.25| 0.50| 97| 10.13| 0.000|   ***|
#> 
#> 4 missing case(s) omitted. Adjusted significance p values with fdr method.
#> 
```
