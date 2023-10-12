library(testthat)

# Test that the function returns a data frame
test_that("lich_occurrences returns a data frame", {
  result <- lich_occurrences("Cetraria ericetorum Opiz")
  expect_s3_class(result, "data.frame")
})

# Test that the function throws an error if the input is not a  a string
test_that("lich_occurrences throws an error if the input is not a string", {
  expect_error(lich_occurrences(c("Cetraria ericetorum Opiz", "Lecanora ciliata")))
})