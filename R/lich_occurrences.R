#' @title Lichen occurrences
#' @description This function returns the occurrences of the lichen species passed as input.
#' @param sp_name A string containing the scientific name of the lichen.
#' @param only_genus A boolean value, if TRUE the occurrences of all the species of the input genus are returned.
#' @return A dataframe containing the occurrences of the lichen species passed as input.
#' @examples
#' lich_occurrences("Cetraria ericetorum Opiz")
#'
#' @import utils
#' @import httr
#' @import jsonlite
#'
#' @export
lich_occurrences <- function(sp_name, only_genus = FALSE) {
  
  # change to a post function (the api stays the same)
  
  # sp_names must be a vector
  if (!is.character(sp_name)) {
    stop("sp_string must be a string")
  }
  # replace Na with empty values
  sp_name <- ifelse(is.na(sp_name), "", sp_name)
  
 
      sp_name <- URLencode(sp_name)
      
      url <- "https://italic.units.it/api/v1/occurrences/"
      parameters <- '?points-only=true'
      if (only_genus) {
        parameters <- paste(parameters, '&is-genus=true', sep = '')
      }
      url <- paste(url, sp_name, sep = '')
      url <- paste(url, parameters, sep = '')
      
      response <- GET(url)
      
      # Deal with api errors
      # 500 server not available (blocks the function)
      # 429 API usage limit exceeded
      if (response$status_code == 500) {
        stop("Impossible to connect to the server, please try again later")
      } else if (response$status_code == 429) {
        stop("Impossible to connect to the server, please try again later")
      } else if (response$status_code == 200) {
        success <- TRUE
      } else {
        stop("An unknown error occurred, please try again later")
      }
      
    # If status_code = 200 everything is fine
    data <- fromJSON(rawToChar(response$content))
    
    input <- as.data.frame(data[1])
    occurrences <- data[3]
    occurrences <- as.data.frame(occurrences$data)
    #warnings <-  as.data.frame(data[4])
    
  return(occurrences)
  
}
