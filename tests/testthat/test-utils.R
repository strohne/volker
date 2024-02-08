library(testthat)
library(volker)

# Test Gini calculation
test_that("Gini coefficient is as expected", {
  expect_equal(
    gini(c(10,10,10,10,10)),
    0
  )

  expect_equal(
    gini(c(0,1)),
    1
  )

  expect_equal(
    gini(c(0,0,1,1)),
    2 / 3
  )

  expect_equal(
    gini(c(10,20,30,40,50)),
    1/3
  )

  expect_equal(
    gini(c(10,20,30.5,40,50)),
    0.33222591
  )
})
