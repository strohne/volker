# Tests for markdown rendering

library(tidyverse)
library(testthat)
library(volker)
library(knitr)

# Rendering tables
test_that("Tables in markdown documents render without error", {
  expect_snapshot_file(rmarkdown::render("_markdown/tables.Rmd"))
})

# Rendering plots
test_that("Plots in markdown documents render without error", {
  expect_snapshot_file(rmarkdown::render("_markdown/plots.Rmd"))
})

# Rendering reports
test_that("Volker reports render without error", {
  expect_snapshot_file(rmarkdown::render("_markdown/reports.Rmd"))
})
