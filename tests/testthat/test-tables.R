#
# Test table appearance
#

library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt

# Frequency table
# tab_counts_one
test_that("Frequency table", {
  expect_snapshot(volker::tab_counts(data, sd_gender), cran = TRUE)
})


# Cross table of categorical variables
# tab_counts_one_grouped
test_that("Cross table of categorical variables", {
  expect_snapshot(volker::tab_counts(data, adopter, sd_gender), cran = TRUE)
})

# Frequency table for multiple categorical variables
# tab_counts_items
test_that("Frequency table for multiple categorical variables", {
  expect_snapshot(
    volker::tab_counts(
      data, tidyselect::starts_with("cg_adoption_")), cran = TRUE)
})

# Frequency table for multiple categorical variables grouped
# tab_counts_items_grouped
test_that("Frequency table for multiple categorical variables by grouping variable", {
  expect_snapshot(
    volker::tab_counts(
      data,
      tidyselect::starts_with("cg_adoption_"),
      sd_gender,
      category = c("agree", "strongly agree")),
      cran = TRUE)
})

# Distribution table for age
# tab_metrics_one
test_that("Distribution table for age", {
  expect_snapshot(volker::tab_metrics(data, sd_age), cran = TRUE)
})

# Group comparison of a metric variable
# tab_metrics_one_grouped
test_that("Group comparison of a metric variable", {
  expect_snapshot(volker::tab_metrics(data, sd_age, sd_gender), cran = TRUE)
})

# Correlations of two variables
# tab_metrics_one_cor
test_that("Compare means of multiple items", {
  expect_snapshot(
    volker::tab_metrics(
      data, use_work, use_private,
      metric=TRUE), cran = TRUE)
})


# Distribution table for multiple metric items
# tab_metrics_items
test_that("Distribution table for multiple metric items", {
  expect_snapshot(
    volker::tab_metrics(
      data, tidyselect::starts_with("cg_adoption_")
      ),cran = TRUE)
})

# Compare means of multiple items
# tab_metrics_items_grouped
test_that("Compare means of multiple items", {
  expect_snapshot(
    volker::tab_metrics(
      data,
      tidyselect::starts_with("cg_adoption_"),
      sd_gender
      ), cran = TRUE)
})


# Correlations of an item battery with one variable
# tab_metrics_items_cor
test_that("Correlations of an item battery with one variable", {
  expect_snapshot(
    volker::tab_metrics(
      data,
      tidyselect::starts_with("cg_adoption_"),
      sd_age, metric=TRUE
      ), cran = TRUE)
})

# Correlations of an item battery with one variable and spearman
# tab_metrics_items_cor
test_that("Correlations of an item battery with one variable and spearman", {
  expect_snapshot(
    volker::tab_metrics(
      data,
      tidyselect::starts_with("cg_adoption_"),
      sd_age, metric=TRUE, method = "spearman"
    ), cran = TRUE)
})

# Correlations of two item batteries
# tab_metrics_items_cor_items
test_that("Correlate two item batteries", {
  expect_snapshot(
    volker::tab_metrics(
      data,
      tidyselect::starts_with("cg_adoption_"),
      tidyselect::starts_with("use_"),
      metric=TRUE
      ),cran = TRUE)
})


# Correlations of two item batteries with ci
# tab_metrics_items_cor_items
test_that("Correlate two item batteries with ci", {
  expect_snapshot(
    volker::tab_metrics(
      data, starts_with("cg_adoption_adv"),
      starts_with("use_"),
      metric = TRUE, ci = T
    ),cran = TRUE)
})

# Correlations of two item batteries and spearman
# tab_metrics_items_cor_items
test_that("Correlate two item batteries with spearmn", {
  expect_snapshot(
    volker::tab_metrics(
      data,
      tidyselect::starts_with("cg_adoption_"),
      tidyselect::starts_with("use_"),
      metric=TRUE, method = "spearman",
    ),cran = TRUE)
})


# ...with missings
test_that("missing values make no trouble", {
  data %>%
#    volker::labs_store() |>
    dplyr::bind_rows(tibble::tibble(sd_gender = c("X", "X", "X"))) %>%
#    volker::labs_restore() |>
    volker::tab_metrics_items_grouped(
      tidyselect::starts_with("cg_adoption_"), sd_gender) %>%
    expect_snapshot(cran = TRUE)
})

# Item order reflects column order
test_that("Item order is kept", {
  expect_snapshot(

    tibble::tribble(
      ~f1, ~f2, ~f10,
      1,     1,    1,
      1,     2,    1,
      2,     2,    2
    ) |>
      tab_counts_items(c(f1:f10))

    , cran = TRUE
  )
})
