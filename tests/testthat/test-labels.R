# Tests for table appearance
library(tidyverse)
library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt
data <- mutate(data, across(starts_with("cg_adoption_"), ~ na_if(., -9)))


# Get labels
test_that("Labels", {
  expect_snapshot(volker::get_labels(data))
})

# What happens when labels are empty?
test_that("Missing labels", {
  data %>%
    dplyr::select(starts_with("cg_adoption")) %>%
    volker::remove_labels() %>%
    volker::get_labels() %>%
    expect_snapshot()
})




