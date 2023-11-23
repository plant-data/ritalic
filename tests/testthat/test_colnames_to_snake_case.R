library(testthat)

# Test that the function changes column names to snake_case
test_that("colnames_to_snake_case changes column names to snake_case", {
  # Create a sample dataframe with column names containing spaces and dots
  df <- data.frame("Column Name 1" = 1:5, "Column.Name.2" = 6:10)
  
  # Apply the function to the dataframe
  result <- colnames_to_snake_case(df)
  
  # Check if the column names have been changed to snake_case
  expect_equal(colnames(result), c("column_name_1", "column_name_2"))
})