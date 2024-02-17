# Tests for labeling functions

library(tidyverse)
library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt
data <- prepare(data)

# Get labels
test_that("Labels", {
  expect_snapshot(volker::codebook(data))
})

# What happens when labels are empty?
test_that("Missing labels", {
  data %>%
    dplyr::select(starts_with("cg_adoption")) %>%
    volker::labs_clear() %>%
    volker::codebook() %>%
    expect_snapshot()
})

# Detect the scale
test_that("Factors are unordered", {
  data %>%
    get_direction(adopter) |>
    expect_equal(0)
})

test_that("Items are ordered", {
  data %>%
    get_direction(use_private) |>
    expect_equal(1)
})

# Test store and clear labels
test_that("Store and clear the codebook", {
  data %>%
    volker::labs_store() %>%
    volker::labs_clear() %>%
    codebook() %>%
    expect_snapshot()
})

# Test store and restore labels
test_that("Store, clear and restore the codebook", {
  data %>%
    volker::labs_store() %>%
    volker::labs_clear() %>%
    volker::labs_restore() %>%
    codebook() %>%
    expect_snapshot()
})

# Replace item labels
test_that("Item labels are replaced and keep their order", {

  # TODO: even if the column was converted to character beforehand
  data |>
    dplyr::select(adopter) |>
    #  dplyr::mutate(adopter = as.character(adopter)) |>
    volker:::labs_replace_values(adopter, volker::codebook(data)) |>
    dplyr::pull(adopter) |>
    levels() |>
    expect_snapshot()
})
