#' Prepare species names for API requests
#' @param sp_names Vector of species names
#' @return Cleaned vector of species names
#' @noRd
prepare_species_names <- function(sp_names) {
  # Input is a character vector
  if (!is.character(sp_names) && !is.vector(sp_names)) {
    stop("sp_names must be a character vector")
  }
  
  # remove invisible characters
  invisible_char_regex <-
    "[\u00AD\u034F\u200B-\u200F\u2028-\u202E\u2060-\u206F\uFEFF]"
  sp_names <- gsub(invisible_char_regex, "", sp_names, perl = TRUE)
  
  # Replace NA with empty strings
  sp_names <- ifelse(is.na(sp_names), "", sp_names)
  
  return(sp_names)
}

#' Reconstruct a dataframe with same order and duplicates as a vector
#'
#' Given a vector with repeated values and a dataframe with a column of original
#' values, reconstruct the dataframe to have the same order and duplicates as the vector.
#'
#' @param original_vector A vector containing the original values to use for ordering
#' and duplication.
#' @param result_dataframe A dataframe containing a column of values to be ordered and duplicated.
#' @param column_with_vector_values The index of the column in the dataframe
#' that contains the values to be ordered and duplicated.
#'
#' @return A dataframe with the same order and duplicates as the original vector.
#' @noRd
reconstruct_order <-
  function(original_vector,
           result_dataframe,
           column_with_vector_values) {
    ordered_dataframe <-
      data.frame(matrix(
        nrow = length(original_vector),
        ncol = ncol(result_dataframe)
      ))
    colnames(ordered_dataframe) <- colnames(result_dataframe)
    
    for (i in 1:length(original_vector)) {
      ordered_dataframe[i,] <-
        result_dataframe[result_dataframe[, column_with_vector_values] == original_vector[i],]
    }
    
    ordered_dataframe <- colnames_to_snake_case(ordered_dataframe)
    
    return(ordered_dataframe)
  }

#' Change columns of a dataframe to snake_case
#' The json returned from Italic APIs generates a dataframe with white spaces (or dots) in column names
#' this function turn the column names to snake_case
#' @param dataframe A dataframe
#' @return A dataframe with column names written in snake_case
#' @noRd

colnames_to_snake_case <- function(dataframe) {
  colnames(dataframe) <- gsub("\\s+", " ", colnames(dataframe))
  colnames(dataframe) <- trimws(colnames(dataframe))
  
  colnames(dataframe) <- tolower(colnames(dataframe))
  colnames(dataframe) <- gsub(" ", "_", colnames(dataframe))
  colnames(dataframe) <- gsub("-", "_", colnames(dataframe))
  colnames(dataframe) <- gsub("'", "_", colnames(dataframe))
  colnames(dataframe) <- gsub("\\.", "_", colnames(dataframe))
  return(dataframe)
}