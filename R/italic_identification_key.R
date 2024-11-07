#' @title Lichen identification key
#' @description This function returns a identification key to the lichen species passed as input.
#' @param sp_names A vector containing scientific names of lichens.
#' @return A link to the online key of italic
#' @import httr
#' @import jsonlite
#' @examples
#' italic_identification_key(c("Cetraria ericetorum Opiz","Xanthoria parietina (L.) Th. Fr."))
#'
#' @export

italic_identification_key <- function(sp_names) {
  
  sp_names <- sp_names[!is.na(sp_names) & nzchar(trimws(sp_names))]
  
  # Check if the resulting vector is empty
  if (length(sp_names) == 0) {
    stop("Error: Vector is empty after removing NA and empty strings")
  }
  
  url <- "https://italic.units.it/api/v1/taxa-records"
  headers <- c('Content-Type' = 'application/json')
  body <- sp_names
  
  
  # Send POST request to API
  response <-
    POST(url,
         body = jsonlite::toJSON(body),
         encode = "json",
         add_headers(headers))
  
  if (http_error(response)) {
    message("HTTP error: ", http_status(response)$message)
    return(NULL)
  }
  
  # Parse the JSON response
  parsed_response <- content(response, "parsed")
  
  if (!is.null(parsed_response$`key-id`)) {
    unique_id <- parsed_response$`key-id`
    return(paste('https://italic.units.it/key-maker/', unique_id, '/nodes/1/species', sep = ''))
  } else {
    message("key-id not found in the response")
    return(parsed_response)
  }
}
