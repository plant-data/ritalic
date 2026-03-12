#' Generate interactive identification keys for lichen taxa
#'
#' @description
#' Creates a URL link to a custom interactive dichotomous key for identifying the specified lichen taxa using
#' the KeyMaker system of ITALIC. Only accepts names that exist in the database of ITALIC.
#'
#' @note Before using this function with a list of names, first obtain their accepted names
#'       using `italic_match()`.
#'       Example workflow:
#'       \preformatted{
#'       names_matched <- italic_match(your_names)
#'       key_url <- italic_identification_key(names_matched$accepted_name)
#'       }
#'
#' @param sp_names Character vector of accepted names
#'
#' @return Character string containing the URL to a web-based interactive identification key.
#'        The key is uniquely generated for the input taxa and allows step-by-step
#'        identification through dichotomous choices.
#'
#' @examples
#' \dontrun{
#' # Generate key for two species
#' italic_identification_key(c("Cetraria ericetorum Opiz","Xanthoria parietina (L.) Th. Fr."))
#' }
#'
#' @references
#' ITALIC - The KeyMaker
#' \url{https://italic.units.it/key-maker/}
#'
#' @importFrom jsonlite fromJSON
#' @export

italic_identification_key <- function(sp_names) {
  sp_names <- sp_names[!is.na(sp_names) & nzchar(trimws(sp_names))]
  
  
  if (length(sp_names) == 0) {
    stop("Error: Vector is empty after removing NA and empty strings")
  }
  
  url <- "https://italic.units.it/api/v1/taxa-records"
  body <- sp_names
  
  response <- httr2::request(url) |>
    httr2::req_headers('Content-Type' = 'application/json') |>
    httr2::req_body_json(body) |>
    httr2::req_perform()
  
  if (httr2::resp_status(response) >= 400) {
    message("Request failed: ", httr2::resp_status_desc(response))
    return(NULL)
  }
  
  parsed_response <- fromJSON(httr2::resp_body_string(response))
  
  if (!is.null(parsed_response$`key-id`)) {
    unique_id <- parsed_response$`key-id`
    return(
      paste(
        'https://italic.units.it/key-maker/',
        unique_id,
        '/nodes/1/interactive',
        sep = ''
      )
    )
  } else {
    message("key-id not found in the response")
    return(parsed_response)
  }
}
