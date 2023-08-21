#' Match species names to their corresponding accepted names in the ITALIC database
#'
#' Given a list of species names, this function matches each name to its corresponding accepted name in the ITALIC database. The function uses the ITALIC API to perform the matching. The function can also accept optional arguments to match at the subspecies, variety, form, or cultivar level.
#'
#' @param area a character vector of species names to be matched
#' @return a list of matched names, with the original names as the names of the list and the matched names as the values
#'
#' @examples
#' get_checklist_lichen('Molise')
#'
#' @import httr
#' @import jsonlite
#'
#' @export
get_checklist_lichen <- function(area) {
  # sp_names must be a character
  if (!is.character(area)) {
    stop("area must be a string or a character")
  }
  # replace Na with empty values
  area <- ifelse(is.na(area), "", area)
  
  area <- URLencode(area)
  
  url <- "https://italic.units.it/api/v1/checklist/"
  url <- paste(url, area, sep = '')
  
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
  checklist <- data[2]
  checklist <- checklist$checklist
  warnings <-  data[3]
  # get warnings if present
  if (warnings != '') {
    warning(warnings)
  }
  
  
  
  
  return(checklist)
  
}
