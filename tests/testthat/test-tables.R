# Test table appearance

library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt
#data <- volker::data_clean(data)

# Frequency table
test_that("Frequency table", {
  expect_snapshot(volker::tab_counts_one(data, sd_gender))
})

# Distribution table for age
test_that("Distribution table for age", {
  expect_snapshot(volker::tab_metrics_one(data, sd_age))
})

# Frequency table for multiple categorical variables
test_that("Frequency table for multiple categorical variables", {
  expect_snapshot(volker::tab_counts_items(data, tidyselect::starts_with("cg_adoption_"), missings = T))
})

# Distribution table for multiple metric items
test_that("Distribution table for multiple metric items", {
  expect_snapshot(volker::tab_metrics_items(data, tidyselect::starts_with("cg_adoption_")))
})

# Cross table of categorical variables
test_that("Cross table of categorical variables", {
  expect_snapshot(volker::tab_counts_one_grouped(data, adopter, sd_gender))
})

# Group comparison of a metric variable
test_that("Group comparison of a metric variable", {
  expect_snapshot(volker::tab_metrics_one_grouped(data, sd_age, sd_gender))
})

# Compare means of multiple items
test_that("Compare means of multiple items", {
  expect_snapshot(volker::tab_metrics_items_grouped(data, tidyselect::starts_with("cg_adoption_"), sd_gender))
})

# ...with missings
test_that("Missing values make no trouble", {
  data %>%
    dplyr::bind_rows(tibble::tibble(sd_gender = c("X", "X", "X"))) %>%
    volker::tab_metrics_items_grouped(tidyselect::starts_with("cg_adoption_"), sd_gender) %>%
    expect_snapshot()
})

# Correlation of items
test_that("Correlation of items", {
  expect_snapshot(volker::tab_metrics_items_cor(data, tidyselect::starts_with("cg_adoption_")))
})

