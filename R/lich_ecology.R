#' Match species names to their corresponding accepted names in the ITALIC database
#'
#' Given a list of species names, this function matches each name to its corresponding accepted name in the ITALIC database. The function uses the ITALIC API to perform the matching. The function can also accept optional arguments to match at the subspecies, variety, form, or cultivar level.
#'
#' @param sp_names a character vector of species names to be matched
#' @return a list of matched names, with the original names as the names of the list and the matched names as the values
#'
#' @examples
#' lich_ecology(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#'
#' @import utils
#' @import httr
#' @import jsonlite
#'
#' @export
lich_ecology <-function(sp_names) {
  
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
      sp_name <- URLencode(sp_name)
      
      url <- "https://italic.units.it/api/v1/ecology/"
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
    data <- fromJSON(rawToChar(response$content))
    
    input <- as.data.frame(data[1])
    ecology <- data[2]
    ecology <- as.data.frame(ecology$ecology)
    warnings <-  as.data.frame(data[3])
    
    result <- cbind(input,ecology,warnings)
    
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
