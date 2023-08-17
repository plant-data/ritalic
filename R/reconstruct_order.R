#' Reconstruct a Dataframe with Same Order and Duplicates as a Vector
#'
#' Given a vector with repeated values and a dataframe with a column of original
#' values, reconstruct the dataframe to have the same order and duplicates as the vector.
#'
#' @param original_vector A vector containing the original values to use for ordering
#' and duplication.
#' @param dataframe A dataframe containing a column of values to be ordered and duplicated.
#' @param column_with_vector_values The index of the column in the dataframe
#' that contains the values to be ordered and duplicated.
#'
#' @return A dataframe with the same order and duplicates as the original vector.
reconstruct_order <-  function(original_vector, dataframe, column_with_vector_values) {
  # Use match() function to get indices of original values in vector
  ordered_dataframe <- data.frame(matrix(nrow = length(original_vector), ncol = ncol(dataframe)))
  colnames(ordered_dataframe) <- colnames(dataframe)

  # Copy values for duplicates
  for(i in 1:length(original_vector)) {
    ordered_dataframe[i,] <- dataframe[dataframe[,column_with_vector_values] == original_vector[i],]
  }
  return(ordered_dataframe)
}
