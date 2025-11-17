# Plot regression coefficients

The regression output comes from
`stats::`[`lm`](https://rdrr.io/r/stats/lm.html).

**\[experimental\]**

## Usage

``` r
model_metrics_plot(
  data,
  col,
  categorical,
  metric,
  interactions = NULL,
  diagnostics = FALSE,
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
  should be provided as multiplication of the variables. Example:
  `c(sd_gender * adopter)`.

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

A volker list object containing volker plots

## Examples

``` r
library(volker)
data <- volker::chatgpt

data |>
  filter(sd_gender != "diverse") |>
  model_metrics_plot(use_work, categorical = c(sd_gender, adopter), metric = sd_age)

```
