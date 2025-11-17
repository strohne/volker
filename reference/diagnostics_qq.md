# Normal Q-Q

Normal Q-Q

## Usage

``` r
diagnostics_qq(fit)
```

## Arguments

- fit:

  The lm fit object

## Value

A ggplot object

## Examples

``` r
library(volker)
data <- filter(volker::chatgpt, sd_gender != "diverse")

data <- add_model(data, use_work, metric = sd_age)

fit <- attr(data$prd_use_work, "lm.fit")
diagnostics_qq(fit)

```
