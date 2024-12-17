library(testthat)

describe("prepare_species_names function", {
  test_that("handles normal character vector", {
    input <- c("Species1", "Species2")
    result <- prepare_species_names(input)
    expect_equal(result, input)
  })
  
  test_that("removes invisible characters", {
    input <- c("Species\u00AD1", "Species\u200B2")
    result <- prepare_species_names(input)
    expect_equal(result, c("Species1", "Species2"))
  })
  
  test_that("replaces NA with empty string", {
    input <- c("Species1", NA, "Species3")
    result <- prepare_species_names(input)
    expect_equal(result, c("Species1", "", "Species3"))
  })
  
})




describe("reconstruct_order function", {
  test_that("maintains original order of input vector", {
    original_vector <- c("Species3", "Species1", "Species2")
    result_dataframe <- data.frame(species = c("Species1", "Species2", "Species3"))
    
    result <-
      reconstruct_order(original_vector, result_dataframe, "species")
    
    expect_equal(result$species, original_vector)
  })
  
  test_that("handles repeated species names", {
    original_vector <- c("Species1", "Species1", "Species2")
    result_dataframe <- data.frame(species = c("Species2", "Species1"))
    
    result <-
      reconstruct_order(original_vector, result_dataframe, "species")
    
    expect_equal(result$species, original_vector)
  })
})