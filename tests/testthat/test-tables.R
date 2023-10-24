# Tests for table appearance
library(tidyverse)
library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt
data <- mutate(data, across(starts_with("cg_adoption_"), ~ na_if(.,-9)  ))

# Frequency table
test_that("frequency table appearance", {
  expect_snapshot(volker::tab_var_counts(data, sd_geschlecht))
})
