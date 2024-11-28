#' Call API for base function
#' @description Call the API endpoint for basic function and organize the result 
#' @param sp_names A vector containing scientific names of lichens
#' @param api_endpoint The API endpoint to call
#' @return A dataframe containing the data from the API endpoint
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @noRd
call_api_base <- function(sp_names, api_endpoint) {
  # Prepare and validate input
  sp_names <- prepare_species_names(sp_names)
  unique_sp_names <- unique(sp_names)
  
  # Initialize progress bar
  pb <- create_progress_bar(
    length(unique_sp_names), 
    "Processing species names..."
  )
  
  # Pre-allocate results list
  results_list <- vector("list", length(unique_sp_names))
  
  # Process each unique species
  for (i in seq_along(unique_sp_names)) {
    # Prepare URL
    sp_name <- URLencode(unique_sp_names[i], reserved = TRUE)
    url <- paste0(api_endpoint, sp_name)
    
    # Make API request
    response <- make_request(
      method = "GET",
      url = url
    )
    
    # Parse response
    results_list[[i]] <- parse_api_response(response)
    
    # Update progress
    update_progress(pb, i)
  }
  
  # Close progress bar
  close_progress_bar(pb)
  
  # Combine results using do.call(rbind, ...)
  result_merged <- do.call(rbind, results_list)
  row.names(result_merged) <- NULL  # Reset row names
  
  # Restore original order and return
  ordered_dataframe <- reconstruct_order(sp_names, result_merged, 1)
  return(ordered_dataframe)
}

#' Parse API response for base function
#' @param response API response object
#' @return Parsed dataframe
#' @noRd
parse_api_response <- function(response) {
  # Parse JSON response
  json_data <- fromJSON(rawToChar(response$content))
  
  # Extract and process input data
  input <- as.data.frame(json_data[1])
  
  # Extract and process species data
  data <- json_data[3]
  data <- lapply(data$data, function(x) if (is.null(x)) NA else x)
  data <- as.data.frame(data)
  
  # Combine input and data
  result <- cbind(input, data)
  return(result)
}