#
# Test plot features
#

library(vdiffr)
library(testthat)
library(volker)

# By default, if vdiffr is not installed, all visual tests are skipped unless
# VDIFFR_RUN_TESTS is explicitly set to "true", which should be the case only on
# a GitHub Actions CI runner with stable version of R.

if (requireNamespace("vdiffr", quietly = TRUE) && utils::packageVersion('testthat') >= '3.0.3') {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  # If vdiffr is not available and visual tests are explicitly required, raise error.
  if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) {
    abort("vdiffr is not installed")
  }

  # Otherwise, assign a dummy function
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}

# Load the sample data
data <- volker::chatgpt

# Only run plot tests if explicitly configured
# because they will fail on different machines due
# to different rendering engines and fonts.
# To enable, call oonce on your machine:
#
#   Sys.setenv("R_LOCALTESTS" = "1")
#
# To disable, call:
#
# Sys.unsetenv("R_LOCALTESTS")
#


#if (Sys.getenv("R_LOCALTESTS") == "1") {

  test_that("boxplot", {

    p <- plot_metrics(data, sd_age, box = T)
    expect_doppelganger("Univariable box plot", p)

    # Save the plot to a temporary file
    #plot_file <- tempfile(fileext = ".png")
    #ggsave(plot_file, plot = p)

    # Capture the plot as a snapshot
    #expect_snapshot_file(plot_file, "plot_metrics_box.png")

  })

  test_that("Univariable denisity plot with CI", {

    p <- plot_metrics(data, sd_age, ci = TRUE)
    expect_doppelganger("Univariable density plot with CI", p)

    # # Save the plot to a temporary file
    # plot_file <- tempfile(fileext = ".png")
    # ggsave(plot_file, plot = p)
    #
    # # Capture the plot as a snapshot
    # expect_snapshot_file(plot_file, "plot_metrics_ci.png")

  })

  test_that("Univariable bar plot with CI", {

    p <- plot_counts(data, sd_gender, ci = TRUE)
    expect_doppelganger("Univariable bar plot with CI", p)
    # # Save the plot to a temporary file
    # plot_file <- tempfile(fileext = ".png")
    # ggsave(plot_file, plot = p)
    #
    # # Capture the plot as a snapshot
    # expect_snapshot_file(plot_file, "plot_counts_ci.png")

  })

#}

test_that("Empty plots are empty", {
   test_data <- tibble(var1=c(NA))

   plot_counts(test_data, var1) |>
     expect_error("Check your data: Are they empty?")

})
