#
# Test labeling functions
#

library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt

# Data_prepare
test_data <- tibble::tibble(var1 = c(1, 2, -1, -9),
                            var2 = c(4, 5, -3, -9))

test_that("data_prepare works correctly", {
  # Test 1: Cleaning, removing missings and negatives without metric_cross
  data_prepare(test_data, var1, var2, negative = FALSE, clean = TRUE) %>%
  expect_snapshot(cran = TRUE)

  # Test 2: Without cleaning but removing missings and negatives
  data_prepare(test_data, var1, var2, negative = FALSE, clean = FALSE) %>%
  expect_snapshot(cran = TRUE)

  # Test 3: With metric_cross enabled
  data_prepare(test_data, var1, var2, negative = FALSE, clean = TRUE, metric_cross = TRUE) %>%
  expect_snapshot(cran = TRUE)

  # Test 4: With negatives kept
  data_prepare(test_data, var1, var2, negative = TRUE, clean = TRUE) %>%
  expect_snapshot(cran = TRUE)
})

# Remove residual negative values
test_that("Residual negatives values are removed", {

  tibble::tibble(var1 = c(1,2,-1,-9)) |>
    data_clean() |>
    expect_snapshot(cran= TRUE)

})

# Remove all negative values
test_that("All negatives are removed", {

  tibble::tibble(var1 = c(1,2,-1,-9)) |>
    data_rm_negatives(var1) |>
    expect_snapshot(cran= TRUE)
})


# Keep negative values
test_that("Negatives are kept", {
  options("vlkr.na.numbers"=FALSE)
  tibble::tibble(var1 = c(1,2,-9)) |>
    data_clean() |>
    expect_snapshot(cran= TRUE)
  options("vlkr.na.numbers"=c(-9))
})

# Get baseline

test_that("Baseline is extracted", {
  result <- volker::tab_counts(data,
    tidyselect::starts_with("cg_adoption_"), sd_gender,
    category = c("agree", "strongly agree")
 )
  get_baseline(result) |>
    expect_snapshot(cran= TRUE)

})
