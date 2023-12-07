#' Change columns of a dataframe to snake_case
#'
#' the json returned from Italic APIs generates a dataframe with white spaces (or dots) in column names
#' this function turn the column names to snake_case
#'
#' @param dataframe A dataframe
#'
#' @return A dataframe with column names written in snake_case

colnames_to_snake_case <- function(dataframe) {
  
  # Remove duplicated whitespaces
  colnames(dataframe) <- gsub("\\s+", " ", colnames(dataframe))
  # Remove leading and trailing whitespaces
  colnames(dataframe) <- trimws(colnames(dataframe))
  
  colnames(dataframe) <- tolower(colnames(dataframe))
  colnames(dataframe) <- gsub(" ", "_", colnames(dataframe))
  colnames(dataframe) <- gsub("\\.", "_", colnames(dataframe))
  return(dataframe)
}