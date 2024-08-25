#
# Skim tests
#

library(testthat)
library(volker)

# Load and recode data
data <- volker::chatgpt

test_that("skim_metrics",{
         skim_metrics(data)
         expect_snapshot(data, cran = TRUE)
})
