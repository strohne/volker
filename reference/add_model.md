# Add a column with predicted values from a regression model

The regression output comes from
`stats::`[`lm`](https://rdrr.io/r/stats/lm.html). The effect sizes are
calculated by
`heplots::`[`etasq`](https://friendly.github.io/heplots/reference/etasq.html).
The variance inflation is calculated by
`car::`[`vif`](https://rdrr.io/pkg/car/man/vif.html).

**\[experimental\]**

## Usage

``` r
add_model(
  data,
  col,
  categorical,
  metric,
  interactions = NULL,
  labels = TRUE,
  clean = TRUE,
  ...
)
```

## Arguments

- data:

  A tibble.

- col:

  The target column holding metric values.

- categorical:

  A tidy column selection holding categorical variables.

- metric:

  A tidy column selection holding metric variables.

- interactions:

  A vector of interaction effects to calculate. Each interaction effect
  should be provided as multiplication of the variables. The interaction
  effect can be provided as character value (e.g.
  `c("sd_gender * adopter")`) or as unquoted column names (e.g.
  `c(sd_gender * adopter)`).

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

The input tibble with one additional column. The new column name is
derived from the target column, prefixed with "prd\_". The new column
will have an attribute "lm.fit" with the fit model.

## Examples

``` r
library(volker)
data <- filter(volker::chatgpt, sd_gender != "diverse")

data <- data |>
  add_model(use_work, categorical = c(sd_gender, adopter), metric = sd_age)
```
