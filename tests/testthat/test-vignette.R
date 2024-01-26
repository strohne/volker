# Tests for vignette
library(tidyverse)
library(testthat)
library(volker)
library(knitr)

# Rendering tables vignette
test_that("Tables in vignette render without error", {
  expect_no_error({
    result <- rmarkdown::render("_markdown/tables.Rmd")
  })
})

# Rendering plots vignette
test_that("Plots in vignette render without error", {
  expect_no_error({
    result <- rmarkdown::render("_markdown/plots.Rmd")
  })
})
