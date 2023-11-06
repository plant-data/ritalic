library(testthat)

# Test that the function returns a data frame
test_that("lich_checklist returns a vector", {
  result <- lich_checklist("Molise")
  expect_s3_class(result, "vector")
})
