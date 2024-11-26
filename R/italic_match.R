#' Match scientific names against the Checklist of the Lichens of Italy
#' @description Aligns scientific names against the Checklist of the Lichens of Italy
#' @param sp_names A character vector of scientific names
#' @param subsp_marks Markers to match at the subspecies level
#' @param var_marks Markers to match at the variety level
#' @param form_marks Markers to match at the form level
#' @return Dataframe with matched names and scores
#' @importFrom jsonlite fromJSON
#' @export
italic_match <- function(sp_names, subsp_marks = c(), var_marks = c(), form_marks = c()) {
  # Prepare and validate input
  sp_names <- prepare_species_names(sp_names)
  unique_sp_names <- unique(sp_names)
  
  # Initialize progress bar
  pb <- create_progress_bar(length(unique_sp_names), "Processing species matches...")
  
  # Pre-allocate results list
  results_list <- vector("list", length(unique_sp_names))
  
  # Process each species
  for (i in seq_along(unique_sp_names)) {
    # Prepare request body
    body <- list(
      'sp' = unique_sp_names[i],
      'subsp-mark' = subsp_marks,
      'var-mark' = var_marks,
      'form-mark' = form_marks
    )
    
    # Make API request
    response <- make_request(
      method = "POST",
      url = "https://italic.units.it/api/v1/match",
      body = body
    )
    
    # Parse response
    results_list[[i]] <- parse_italic_response(response)
    
    # Update progress
    update_progress(pb, i)
  }
  
  # Close progress bar
  close_progress_bar(pb)
  
  # Combine results using do.call(rbind, ...)
  result_merged <- do.call(rbind, results_list)
  row.names(result_merged) <- NULL  # Reset row names
  
  # Restore original order
  ordered_dataframe <- reconstruct_order(sp_names, result_merged, 1)
  
  return(ordered_dataframe)
}

#' Parse italic API response
#' @param response API response object
#' @return Parsed dataframe
parse_italic_response <- function(response) {
  # Parse JSON response
  data <- fromJSON(rawToChar(response$content))
  
  # Extract and process input data
  input <- as.data.frame(data[1])
  
  # Extract and process match data
  match <- data[2]
  match <- lapply(match$match, function(x) if (is.null(x)) NA else x)
  
  # Combine input and match data
  result <- cbind(input, match)
  return(result)
}