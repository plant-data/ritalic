library(testthat)

# Test that the function returns a data frame
test_that("lich_occurrences returns a data frame", {
  result <- lich_occurrences("Cetraria ericetorum Opiz")
  expect_s3_class(result, "data.frame")
})
