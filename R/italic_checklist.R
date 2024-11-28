#' Get the list of species names in the Checklist of the Lichens of Italy
#'
#' @description
#' Retrieves the complete list of accepted scientific names from the Checklist of
#' the Lichens of Italy in ITALIC. The function returns all accepted names of species occurring in Italy and in bordering countries
#'
#' @return A character vector containing all accepted scientific names from the checklist of ITALIC.
#'
#' @examples
#' \dontrun{
#' # Get the complete checklist
#' checklist <- italic_checklist()
#'
#' # View the first few names
#' head(checklist)
#' }
#'
#' @references
#' ITALIC - The Information System on Italian Lichens: National Checklist
#' \url{https://italic.units.it/index.php?procedure=checklist}
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
italic_checklist <- function() {
  url <- "https://italic.units.it/api/v1/checklist/"
  response <- GET(url)
  
  if (response$status_code == 500) {
    stop("Impossible to connect to the server, please try again later")
  } else if (response$status_code == 429) {
    stop("Impossible to connect to the server, please try again later")
  } else if (response$status_code == 200) {
    success <- TRUE
  } else {
    stop("An unknown error occurred, please try again later")
  }
  

  data <- fromJSON(rawToChar(response$content))
  
  checklist <- data[2]
  checklist <- checklist$checklist
  
  return(checklist)
  
}