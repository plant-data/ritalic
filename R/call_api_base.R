#' Call API for base function
#' @description Call the API endpoint for basic function and organize the result
#' @param sp_names A vector containing scientific names of lichens
#' @param api_endpoint The API endpoint to call
#' @param loading_text Text to display in the progress bar
#' @param parse_function The function to use to parse the API response
#' @param request_method Method for the request. Defaults to "GET". Can be "POST"
#' @param body The body of the request if the method is "POST"
#' @return A dataframe containing the data from the API endpoint
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @noRd
call_api_base <-
  function(sp_names,
           api_endpoint,
           loading_text,
           parse_function,
           request_method = "GET",
           body = NULL,
           extra_param = "",
           reorder_result = TRUE) {
    # prepare and validate input
    sp_names <- prepare_species_names(sp_names)
    unique_sp_names <- unique(sp_names)
    
    pb <- create_progress_bar(length(unique_sp_names), loading_text)
    
    # create results list
    results_list <- vector("list", length(unique_sp_names))
    
    # get data
    for (i in seq_along(unique_sp_names)) {
      sp_name <- unique_sp_names[i]
      
      if (!is.null(body)) {

        body_with_sp <- body
        body_with_sp$sp <- sp_name
        
        response <- make_request(method = request_method,
                                 url = api_endpoint,
                                 body = body_with_sp)
      } else {

        sp_encoded <- URLencode(sp_name, reserved = TRUE)
        url <- if (extra_param != "") {
          paste0(api_endpoint, sp_encoded, extra_param)
        } else {
          paste0(api_endpoint, sp_encoded)
        }
        
        response <- make_request(method = request_method,
                                 url = url,
                                 body = NULL)
      }
      
      results_list[[i]] <- parse_function(response)
      update_progress(pb, i)
    }
    
    close_progress_bar(pb)
    
    # combine results
    result_merged <- do.call(rbind, results_list)
    row.names(result_merged) <- NULL
    
    # restore original order
    if (reorder_result) {
      ordered_dataframe <- reconstruct_order(sp_names, result_merged, 1)
      return(ordered_dataframe)
    }
    return(result_merged)
  }