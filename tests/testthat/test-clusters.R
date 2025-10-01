#
# Test cluster calculations
#

test_that("Clusters are added", {

  set.seed(137)
  data <- tibble::tibble(
    item1 = c(c(1:10), c(20:30)),
    item2 = c(c(1:10), c(20:30))
  )

  data <- suppressWarnings(add_clusters(data, c(item1, item2), k = NULL))

  expect_snapshot({
    volker::cluster_tab(data, cls_item)
  })
})

# test_that("Clusters with missings are added", {
#   options(vlkr.na.omit=FALSE)
#
#   set.seed(137)
#   data_with_missings <- tibble::tibble(
#     f1 = c(1:20),
#     f2 = c(1:18, NA, NA)
#   )
#
#   d <- suppressWarnings(add_clusters(data_with_missings, c(f1, f2), k = NULL))
#
#   expect_snapshot({
#     volker::cluster_tab(d, cls_f)
#   }, cran = TRUE)
#
#   options(vlkr.na.omit=TRUE)
# })
