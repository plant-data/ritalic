#' @title Call api for base function
#' @description Call the api endpoint for basic function and orgaize the result 
#' @param sp_names A vector containing scientific names of lichens.
#' @param api_endpoint The api endpoint to call
#' @return A dataframe containing the data from the api endpoint
#'
#' @import utils
#' @import httr
#' @import jsonlite
#'
call_api_base <- function(sp_names, api_endpoint) {
  
  # sp_names must be a vector
  if (!is.character(sp_names) && !is.vector(sp_names)) {
    stop("sp_string must be a string or a vector")
  } else if (is.character(sp_names)) {
    sp_names <- 
      c(sp_names)
  }
  # replace Na with empty values
  sp_names <- ifelse(is.na(sp_names), "", sp_names)
  
  # create a vector with only unique species names
  unique_sp_names <- unique(sp_names)
  
  # for each unique name call the match api the result is put in a dataframe
  progress_bar <- define_progress_bar(length(unique_sp_names))
  for (i in 1:length(unique_sp_names)) {
    # these parameters are used to retry the iteration after a 429 code
    
    success <- FALSE
    while (!success) {
      
      sp_name <- unique_sp_names[i];
      sp_name <- URLencode(sp_name, reserved = TRUE)
      
      url <- api_endpoint
      url <- paste(url, sp_name, sep = '')
      
      response <- GET(url)
      
      # Deal with api errors
      # 500 server not available (blocks the function)
      # 429 API usage limit exceeded
      if (response$status_code == 500) {
        stop("Impossible to connect to the server, please try again later")
      } else if (response$status_code == 429) {
        # wait the end of the api cooldown and retry
        wait_api_cooldown()
      } else if (response$status_code == 200) {
        success <- TRUE
      } else {
        stop("An unknown error occurred, please try again later")
      }
      
    }
    
    # If status_code = 200 everything is fine
    json_data <- fromJSON(rawToChar(response$content))
    
    input <- as.data.frame(json_data[1])
    data <- json_data[3]
    data <- lapply(data$data, function(x) if (is.null(x)) NA else x)
    data <- as.data.frame(data)
    
    
    result <- cbind(input,data)
    
    if (i == 1) {
      result_merged <- result
    } else {
      result_merged <- rbind(result_merged, result)
    }
    
    utils::setTxtProgressBar(progress_bar, i)
    
  }
  
  # at the end of the cycle, the original array is rebuilt
  ordered_dataframe <- reconstruct_order(sp_names, result_merged, 1)
  return(ordered_dataframe)
  
}