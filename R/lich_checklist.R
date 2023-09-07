#' @title Lichen checklist
#' @description This function returns the checklist of the lichen species present in the area of Italy passed as input. Accepted values for the area parameter are the following:Friuli, Venezia-Giulia, Veneto, Trentino Alto-Adige, Lombardia, Piemonte, Valle d'Aosta, Liguria, Emilia Romagna, Toscana, Umbria, Marche, Abruzzo, Molise, Puglia, Basilicata, Campania, Calabria, Sicilia, Sardegna, Lazio, Italy, Dolomites
#' @param area A string containing one area of Italy.
#' @return A dataframe containing the checklist of the lichen species present in the area of Italy passed as input.
#' @examples
#' lich_checklist('Molise')
#'
#' @import httr
#' @import jsonlite
#'
#' @export
lich_checklist <- function(area) {
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
