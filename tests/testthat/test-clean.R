#
# Test labeling functions
#

library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt

# Data_prepare
test_data <- tibble::tibble(
  var1 = c(1, 2, -1, 5),
  var2 = c(-2, -3, -3, -9)
)

attr(test_data$var1, "-1") <- "LABELED RESIDUAL VALUE"
attr(test_data$var2, "-3") <- "NOT A RESIDUAL VALUE" # NOT in VLKR_NA_NUMBERS, should not be removed
#attr(test_data$var2, "-9") <- "UNLABELED RESIDUAL VALUE" # In VLKR_NA_NUMBERS but not in attributes, should be removed

# Keep residuals
test_that("No values are recoded to missings", {
  prepared_data <- data_prepare(test_data, var1, var2, clean = FALSE)
  expect_snapshot(prepared_data, cran = TRUE)

  expect_true(is.null(attr(prepared_data, "missings")))

})

# Recode residuals
test_that("Residual values are recoded to missings", {
  prepared_data <- data_prepare(test_data, var1, var2, clean = TRUE)
  expect_snapshot(prepared_data, cran = TRUE)

  expect_true(!is.null(attr(prepared_data, "missings")))

})

# Remove residual negative values
test_that("Residual negative values are removed", {

  tibble::tibble(var1 = c(1, 2, -1, -9, -50)) |>
    data_clean() |>
    expect_snapshot(cran = TRUE)

})

# Remove all negative values
test_that("All negatives are removed", {

  tibble::tibble(var1 = c(1,2,-1,-9)) |>
    data_rm_negatives(var1) |>
    expect_snapshot(cran = TRUE)
})


# Keep negative values
test_that("Negatives are kept", {
  options("vlkr.na.numbers"=FALSE)
  tibble::tibble(var1 = c(1,2,-9)) |>
    data_clean() |>
    expect_snapshot(cran = TRUE)
  options("vlkr.na.numbers"= VLKR_NA_NUMBERS)
})

# Get baseline

test_that("Baseline is extracted", {
  result <- volker::tab_counts(data,
    tidyselect::starts_with("cg_adoption_"), sd_gender,
    category = c("agree", "strongly agree")
 )
  get_baseline(result) |>
    expect_snapshot(cran = TRUE)

})
