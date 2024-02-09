# Tests for markdown rendering

library(tidyverse)
library(testthat)
library(volker)
library(knitr)

# Rendering tables
test_that("Tables in markdown documents render without error", {
  expect_snapshot_file(rmarkdown::render("_markdown/tables.Rmd"))
})


# Pixel accuracy is out of scope for our CRAN version by now.
# Markdowns should run without errors, but we don't compare the images.
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  # Rendering plots
  test_that("Plots in markdown documents render without error", {
    expect_snapshot_file(rmarkdown::render("_markdown/plots.Rmd"))
  })

  # Rendering reports
  test_that("Volker reports render without error", {
    expect_snapshot_file(rmarkdown::render("_markdown/reports.Rmd"))
  })
} else {

  # Rendering plots
  test_that("Plots in markdown documents render without error", {
    expect_no_error(rmarkdown::render("_markdown/plots.Rmd"))
  })

  # Rendering reports
  test_that("Volker reports render without error", {
    expect_no_error(rmarkdown::render("_markdown/reports.Rmd"))
  })
}
