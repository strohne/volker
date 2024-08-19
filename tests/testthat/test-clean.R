#
# Test labeling functions
#

library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt


# Remove residual negative values
test_that("Residual negatives values are removes", {

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
