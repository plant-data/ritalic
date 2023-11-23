library(testthat)

# Test that the function returns a data frame
test_that("lich_match returns a data frame", {
  result <- lich_match(c("Cetraria islandica", "Lecanora ciliata"))
  expect_s3_class(result, "data.frame")
})

# Test that the function returns the correct number of rows with a vector as input
test_that("lich_match returns the correct number of rows", {
  result <- lich_match(c("Cetraria islandica", "Lecanora ciliata"))
  expect_equal(nrow(result), 2)
})

# Test that the function returns the correct number of rows with a string as input
test_that("lich_match returns the correct number of rows", {
  result <- lich_match("Cetraria islandica")
  expect_equal(nrow(result), 1)
})

