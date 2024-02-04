# Tests for labeling functions

library(tidyverse)
library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt
data <- prepare(data)


# Get labels
test_that("Labels", {
  expect_snapshot(volker::get_codebook(data))
})

# What happens when labels are empty?
test_that("Missing labels", {
  data %>%
    dplyr::select(starts_with("cg_adoption")) %>%
    volker::remove_labels() %>%
    volker::get_codebook() %>%
    expect_snapshot()
})




