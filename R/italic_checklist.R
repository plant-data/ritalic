#' @title Lichen checklist
#' @description This function returns the checklist of the lichen species present in Italy.
#' @return A vector containing the names of the lichen species present in Italy.
#' @examples
#' lich_checklist()
#'
#' @import httr
#' @import jsonlite
#'
#' @export
lich_checklist <- function() {
 
  url <- "https://italic.units.it/api/v1/checklist/"
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
  
  checklist <- data[2]
  checklist <- checklist$checklist
  
  return(checklist)
  
}