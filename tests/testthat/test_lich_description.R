library(testthat)

# Test that the function returns a data frame
test_that("lich_description returns a data frame", {
  result <- lich_description(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
  expect_s3_class(result, "data.frame")
})

# Test that the function returns the correct number of rows
test_that("lich_description returns the correct number of rows", {
  result <- lich_description(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
  expect_equal(nrow(result), 2)
})

# Test that the function throws an error if the input is not a vector or a string
test_that("lich_description throws an error if the input is not a vector or a string", {
  expect_error(lich_description(1))
})