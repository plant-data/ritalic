#' Get lichen occurrences
#' @description Returns the occurrences of the lichen species passed as input
#' @param sp_names A vector of scientific names of lichens
#' @param result_data Type of data to return: "simple" (default) or "extended"
#' @return A dataframe containing the occurrences data
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @export
italic_occurrences <- function(sp_names, result_data = 'simple') {
  # Prepare and validate input
  sp_names <- prepare_species_names(sp_names)
  unique_sp_names <- unique(sp_names)
  
  # Initialize progress bar
  pb <- create_progress_bar(
    length(unique_sp_names), 
    "Processing occurrences..."
  )
  
  # Pre-allocate results list
  results_list <- vector("list", length(unique_sp_names))
  has_results <- FALSE
  
  # Process each unique species
  for (i in seq_along(unique_sp_names)) {
    # Prepare URL
    sp_name <- URLencode(unique_sp_names[i], reserved = TRUE)
    url <- paste0(
      "https://italic.units.it/api/v1/occurrences/",
      sp_name,
      if(result_data == 'extended') '&result_data=extended' else ''
    )
    
    # Make API request
    response <- make_request(
      method = "GET",
      url = url
    )
    
    # Parse response
    result <- parse_occurrences_response(response)
    
    # Store result if not empty
    if (!is.null(result) && nrow(result) > 0) {
      results_list[[i]] <- result
      has_results <- TRUE
    }
    
    # Update progress
    update_progress(pb, i)
  }
  
  # Close progress bar
  close_progress_bar(pb)
  
  # Handle results
  if (!has_results) {
    return(data.frame())  # Return empty dataframe if no results
  }
  
  # Remove NULL entries and combine results
  valid_results <- Filter(Negate(is.null), results_list)
  result_merged <- do.call(rbind, valid_results)
  row.names(result_merged) <- NULL  # Reset row names
  
  return(result_merged)
}

#' Parse occurrences API response
#' @param response API response object
#' @return Parsed dataframe or NULL if empty
parse_occurrences_response <- function(response) {
  # Parse JSON response
  json_data <- fromJSON(rawToChar(response$content))
  
  # Extract data
  input <- as.data.frame(json_data[1])
  data <- json_data[3]$data
  
  # Handle empty results
  if (is.list(data) && length(data) == 0) {
    return(NULL)
  }
  
  # Convert to dataframe
  result <- as.data.frame(data)
  return(result)
}