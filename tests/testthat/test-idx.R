#
# Test index, cluster and factor
#

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

test_that("factors are added", {
  expect_snapshot({
    data %>% add_factors(
      tidyselect::starts_with("cg_adoption"), k = NULL) %>%
      factor_tab(starts_with("fct_cg_adoption"))
  })
})

test_that("clusters are added", {
  expect_snapshot({
    data %>% add_clusters(
      tidyselect::starts_with("cg_adoption"), k = NULL) %>%
      cluster_tab(cls_cg_adoption)
  })
})

