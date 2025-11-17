# Residuals vs Fitted plot

Residuals vs Fitted plot

## Usage

``` r
diagnostics_resid_fitted(fit)
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
diagnostics_resid_fitted(fit)
```
