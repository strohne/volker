# Tests for vignette
library(tidyverse)
library(testthat)
library(volker)
library(knitr)

# Rendering tables vignette
test_that("Tables in vignette render without error", {
  expect_no_error({
    vignette_path <- system.file("vignettes", "tables.Rmd", package = "volker")
    result <- rmarkdown::render(vignette_path)
  })
})

# Rendering plots vignette
test_that("Plots in vignette render without error", {
  expect_no_error({
    vignette_path <- system.file("vignettes", "plots.Rmd", package = "volker")
    result <- rmarkdown::render(vignette_path)
  })
})
