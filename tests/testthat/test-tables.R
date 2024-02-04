# Test table appearance

library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt
data <- volker::prepare(data)

# Frequency table
test_that("Frequency table", {
  expect_snapshot(volker::tab_counts_one(data, sd_geschlecht))
})

# Distribution table for age
test_that("Distribution table for age", {
  expect_snapshot(volker::tab_metrics_one(data, sd_alter))
})

# Frequency table for multiple categorical variables
test_that("Frequency table for multiple categorical variables", {
  expect_snapshot(volker::tab_counts_items(data, starts_with("cg_adoption_"), missings = T))
})

# Distribution table for multiple metric items
test_that("Distribution table for multiple metric items", {
  expect_snapshot(volker::tab_metrics_items(data, starts_with("cg_adoption_")))
})

# Cross table of categorical variables
test_that("Cross table of categorical variables", {
  expect_snapshot(volker::tab_counts_one_grouped(data, in_adoption, sd_geschlecht))
})

# Group comparison of a metric variable
test_that("Group comparison of a metric variable", {
  expect_snapshot(volker::tab_metrics_one_grouped(data, sd_alter, sd_geschlecht))
})

# Compare means of multiple items
test_that("Compare means of multiple items", {
  expect_snapshot(volker::tab_metrics_items_grouped(data, starts_with("cg_adoption_"), sd_geschlecht))
})

# ...with missings
test_that("Missing values make no trouble", {
  data %>%
    dplyr::bind_rows(tibble(sd_geschlecht = c("X", "X", "X"))) %>%
    volker::tab_multi_means(starts_with("cg_adoption_"), sd_geschlecht) %>%
    expect_snapshot()
})

# Correlation of items
test_that("Correlation of items", {
  expect_snapshot(volker::tab_metrics_items_cor(data, starts_with("cg_adoption_")))
})

