#' Match species names to their corresponding accepted names in the ITALIC database
#'
#' Given a list of species names, this function matches each name to its corresponding accepted name in the ITALIC database. The function uses the ITALIC API to perform the matching. The function can also accept optional arguments to match at the subspecies, variety, form, or cultivar level.
#'
#' @param sp_name a character vector of species names to be matched
#' @param only_genus search for genus instead of species name
#' @return a list of matched names, with the original names as the names of the list and the matched names as the values
#'
#' @examples
#' lich_occurrences("Cetraria ericetorum Opiz")
#'
#' @import utils
#' @import httr
#' @import jsonlite
#'
#' @export
lich_occurrences <-function(sp_name, only_genus = FALSE) {
  
  # sp_names must be a vector
  if (!is.character(sp_name)) {
    stop("sp_string must be a string")
  }
  # replace Na with empty values
  sp_name <- ifelse(is.na(sp_name), "", sp_name)
  
 
      sp_name <- URLencode(sp_name)
      
      url <- "https://italic.units.it/api/v1/occurrences/"
      parameters <- '?only-points=true'
      if (only_genus) {
        parameters <- paste(parameters, '&only-genus=true', sep = '')
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
    occurrences <- data[2]
    occurrences <- as.data.frame(occurrences$occurrences)
    warnings <-  as.data.frame(data[3])
    
  return(occurrences)
  
}
