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

# Test that the function returns the correct column names
test_that("lich_match returns the correct column names", {
  result <- lich_match(c("Cetraria islandica", "Lecanora ciliata"))
  expect_equal(colnames(result), c("input_name", "matched_name", "status", "accepted_name", "score", "name_score", "auth_score"))
})

# Test that the function returns the correct values for NA input
test_that("lich_match returns the correct values for NA", {
  result <- lich_match(NA)
  expect_equal(result$input_name[1], "")
  expect_equal(result$matched_name[1], "")
  expect_equal(result$status[1], "")
  expect_equal(result$accepted_name[1], "")
  expect_equal(result$score[1], 0)
  expect_equal(result$name_score[1], 0)
  expect_equal(result$auth_score[1], 0)
})

# Test that the function returns the correct values for empty input
test_that("lich_match returns the correct values for empty input", {
  result <- lich_match("")
  expect_equal(result$input_name[1], "")
  expect_equal(result$matched_name[1], "")
  expect_equal(result$status[1], "")
  expect_equal(result$accepted_name[1], "")
  expect_equal(result$score[1], 0)
  expect_equal(result$name_score[1], 0)
  expect_equal(result$auth_score[1], 0)
})
