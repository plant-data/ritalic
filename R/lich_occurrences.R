#' @title Lichen occurrences
#' @description This function returns the occurrences of the lichen species passed as input.
#' @param sp_names A string containing the scientific name of the lichen.
#' @return A dataframe containing the occurrences of the lichen species passed as input.
#' @examples
#' lich_occurrences("Cetraria ericetorum Opiz")
#'
#' @import utils
#' @import httr
#' @import jsonlite
#'
#' @export

lich_occurrences <-function(sp_names) {
  
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
    
    # a value to handle potential emty results later
    is_table_yet <- FALSE
    for (i in 1:length(unique_sp_names)) {
      # these parameters are used to retry the iteration after a 429 code
      
      success <- FALSE
      while (!success) {
        
        sp_name <- unique_sp_names[i];
        sp_name <- URLencode(sp_name)
        
        url <- "https://italic.units.it/api/v1/occurrences/"
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
      data <- data$data
      
      # if the result is empty move to the next name
      # if there are only invalid names, return an empty dataframe
      if (is.list(data) && length(data) == 0) {
        if (is_table_yet == FALSE) {
          result_merged = data.frame()
        }
        
        next
      } else {
        
      result <- as.data.frame(data)
      
      if (is_table_yet == FALSE) {
        
        result_merged <- result
        is_table_yet <- TRUE
        
      } else {
        result_merged <- rbind(result_merged, result)
      }
      utils::setTxtProgressBar(progress_bar, i)
      }
    }
    
    return(result_merged)
    
}