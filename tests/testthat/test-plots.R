#
# Test plot features
#

library(vdiffr)
library(testthat)
library(volker)

# If vdiffr is not installed, all visual tests are skipped.
if (requireNamespace("vdiffr", quietly = TRUE) && utils::packageVersion('testthat') >= '3.0.3') {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}

# Only run plot tests if explicitly configured
# because they will fail on different machines due
# to different rendering engines and fonts.
# To enable, call once on your machine (or in a GitHub CI Pipeline):
#
#   Sys.setenv("R_LOCALTESTS" = "1")
#
# To disable, call:
#
# Sys.unsetenv("R_LOCALTESTS")
#

# Load the sample data
data <- volker::chatgpt

if (Sys.getenv("R_LOCALTESTS") == "1") {

  test_that("boxplot renders as expected", {

    p <- plot_metrics(data, sd_age, box = T)
    expect_doppelganger("Univariable box plot", p)
  })

  test_that("Univariable denisity plot with CI", {

    p <- plot_metrics(data, sd_age, ci = TRUE)
    expect_doppelganger("Univariable density plot with CI", p)
  })

  test_that("univariable bar plot with CI renders as expected", {

    p <- plot_counts(data, sd_gender, ci = TRUE)
    expect_doppelganger("Univariable bar plot with CI", p)
  })

}

test_that("Empty plots are empty", {
   test_data <- tibble(var1=c(NA))

   plot_counts(test_data, var1) |>
     expect_error("Check your data: Are they empty?")

})
