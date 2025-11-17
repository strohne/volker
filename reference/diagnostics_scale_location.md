# Scale-Location (Spread-Location)

Scale-Location (Spread-Location)

## Usage

``` r
diagnostics_scale_location(fit)
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

fit <- attr(data$prd_use_work, "lm.fit)
diagnostics_scale_location(fit")
```
