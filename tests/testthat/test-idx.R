#
# Test index functions
#

library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt
set.seed(137)

test_that("idx_add is deprecated", {
  expect_snapshot({
    data %>% idx_add(
      tidyselect::starts_with("cg_adoption")) %>%
      tab_metrics_one_grouped(idx_cg_adoption, adopter)
  })
})

test_that("idx_add is deprecated", {
  expect_snapshot({
    data %>% add_index(
      tidyselect::starts_with("cg_adoption")) %>%
      tab_metrics_one_grouped(idx_cg_adoption, adopter)
  })
})
