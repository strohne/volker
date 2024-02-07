# Test plots work

library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt
data <- volker::prepare(data)

# Single column
test_that("Single column selections work", {
  expect_no_error(
    volker::plot_metrics(data, cg_nutzen_privat)
  )
  expect_no_error(
    volker::tab_metrics(data, cg_nutzen_privat)
  )
  expect_no_error(
    volker::plot_counts(data, cg_nutzen_privat)
  )
  expect_no_error(
    volker::tab_counts(data, cg_nutzen_privat)
  )
})

# Single column in multicolumn functions
test_that("Single column selections work", {
  expect_no_error(
    volker::plot_metrics_items(data, cg_nutzen_privat)
  )
  expect_no_error(
    volker::tab_metrics_items(data, cg_nutzen_privat)
  )
  expect_no_error(
    volker::plot_counts_items(data, cg_nutzen_privat)
  )
  expect_no_error(
    volker::tab_counts_items(data, cg_nutzen_privat)
  )
})

# Multiple columns
test_that("List column selections work", {
  expect_no_error(
    volker::plot_metrics(data, c(cg_nutzen_privat, cg_nutzen_beruflich))
  )
  expect_no_error(
    volker::tab_metrics(data, c(cg_nutzen_privat, cg_nutzen_beruflich))
  )
  expect_no_error(
    volker::plot_counts(data, c(cg_nutzen_privat, cg_nutzen_beruflich))
  )
  expect_no_error(
    volker::tab_counts(data, c(cg_nutzen_privat, cg_nutzen_beruflich))
  )
})

# Multiple columns using starts_with()
test_that("Starts with column selections work", {
  expect_no_error(
    volker::plot_metrics(data, tidyselect::starts_with("cg_nutzen_"))
  )
  expect_no_error(
    volker::tab_metrics(data, tidyselect::starts_with("cg_nutzen_"))
  )
  expect_no_error(
    volker::plot_counts(data, tidyselect::starts_with("cg_nutzen_"))
  )
  expect_no_error(
    volker::tab_counts(data, tidyselect::starts_with("cg_nutzen_"))
  )
})

