library(testthat)

# Test that the function returns a data frame
test_that("lich_checklist returns a data frame", {
  result <- lich_checklist("Molise")
  expect_s3_class(result, "vector")
})

# Test that the function returns the correct number of rows
test_that("lich_checklist returns the correct number of rows", {
  result <- lich_checklist("Molise")
  expect_true(nrow(result) > 0)
})

# Test that the function returns the correct column names
test_that("lich_checklist returns the correct column names", {
  result <- lich_checklist("Molise")
  expect_equal(colnames(result), c("species", "count"))
})

# Test that the function returns the correct values
test_that("lich_checklist returns the correct values", {
  result <- lich_checklist("Molise")
  expect_true(all(result$species != ""))
  expect_true(all(result$count > 0))
})