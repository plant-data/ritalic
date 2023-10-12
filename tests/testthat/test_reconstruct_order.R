library(testthat)

# Test that the function returns a data frame
test_that("reconstruct_order returns a data frame", {
  original_vector <- c("a", "b", "c", "a")
  dataframe <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3))
  result <- reconstruct_order(original_vector, dataframe, 1)
  expect_s3_class(result, "data.frame")
})

# Test that the function returns the correct number of rows
test_that("reconstruct_order returns the correct number of rows", {
  original_vector <- c("a", "b", "c", "a")
  dataframe <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3))
  result <- reconstruct_order(original_vector, dataframe, 1)
  expect_equal(nrow(result), length(original_vector))
})

# Test that the function returns the correct column names
test_that("reconstruct_order returns the correct column names", {
  original_vector <- c("a", "b", "c", "a")
  dataframe <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3))
  result <- reconstruct_order(original_vector, dataframe, 1)
  expect_equal(colnames(result), colnames(dataframe))
})

# Test that the function returns the correct values
test_that("reconstruct_order returns the correct values", {
  original_vector <- c("a", "b", "c", "a")
  dataframe <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3))
  result <- reconstruct_order(original_vector, dataframe, 1)
  expect_equal(result[1,1], original_vector[1])
  expect_equal(result[2,1], original_vector[2])
  expect_equal(result[3,1], original_vector[3])
  expect_equal(result[4,1], original_vector[1])
  expect_equal(result[1,2], dataframe[1,2])
  expect_equal(result[2,2], dataframe[2,2])
  expect_equal(result[3,2], dataframe[3,2])
  expect_equal(result[4,2], dataframe[1,2])
})