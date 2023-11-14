# Tests for vignette
library(tidyverse)
library(testthat)
library(volker)
library(knitr)

# Rendering vignette
test_that("Vignette Test Rendering", {
  expect_no_error({
    result <- rmarkdown::render("vignettes/introduction.Rmd")
  })
})




