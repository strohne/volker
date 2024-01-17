# Tests for table appearance
library(tidyverse)
library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt
data <- mutate(data, across(starts_with("cg_adoption_"), ~ na_if(., -9)))

# Frequency table
test_that("Frequency table", {
  expect_snapshot(volker::tab_var_counts(data, sd_geschlecht))
})

# Distribution table for age
test_that("Distribution table for age", {
  expect_snapshot(volker::tab_var_metrics(data, sd_alter))
})

# Frequency table for multiple categorical variables
test_that("Frequency table for multiple categorical variables", {
  expect_snapshot(volker::tab_item_counts(data, starts_with("cg_adoption_")))
})

# Distribution table for multiple metric items
test_that("Distribution table for multiple metric items", {
  expect_snapshot(volker::tab_item_metrics(data, starts_with("cg_adoption_")))
})

# Cross table of categorical variables
test_that("Cross table of categorical variables", {
  expect_snapshot(volker::tab_group_counts(data, in_adoption, sd_geschlecht))
})

# Group comparison of a metric variable
test_that("Group comparison of a metric variable", {
  expect_snapshot(volker::tab_group_metrics(data, sd_alter, sd_geschlecht))
})

# Compare means of multiple items
test_that("Compare means of multiple items", {
  expect_snapshot(volker::tab_multi_means(data, starts_with("cg_adoption_"), sd_geschlecht))
})

# ...with missings
data %>%
  bind_rows(tibble(sd_geschlecht = c("X", "X", "X"))) %>%
  volker::tab_multi_means(starts_with("cg_adoption_"), sd_geschlecht) %>%
  expect_snapshot()

# Correlation of items
test_that("Correlation of items", {
  expect_snapshot(volker::tab_multi_corr(data, starts_with("cg_adoption_")))
})

