#' Get details for specific names
#'
#' @description
#' Retrieves the morphological description and dditional taxonomic or ecological notes about lichen taxa present in the Checklist of the Lichens of Italy.
#' Only accepts names that exist in the database of ITALIC.
#'
#' @note Before using this function with a list of names, first obtain their matched names or 
#'       accepted names using `italic_match()`.
#'       Example workflow:
#'       \preformatted{
#'       names_matched <- italic_match(your_names)
#'       name_data <- italic_name_usage(names_matched$matched_name)
#'       # or
#'       accepted_name_data <- italic_name_usage(names_matched$accepted_name)
#'       }
#'
#' @param sp_names Character vector of accepted names
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{scientific_name}{Scientific name}
#'     \item{description}{Morphological description}
#'     \item{notes}{Additional taxonomic or ecological information}
#'   }
#'
#' @examples
#' \dontrun{
#' italic_name_usage("Cetraria islandica (L.) Ach. subsp. islandica")
#' }
#'
#'
#' @export
italic_name_usage <- function(sp_names) {
  data <-
    call_api_base(
      sp_names,
      api_endpoint = "https://italic.units.it/api/v1/name-usage/",
      loading_text = "Retrieving name data...",
      parse_function = parse_match_response,
      request_method = "GET",
      reorder_result = TRUE
    )
  return(data)
  
}

#' Parse italic name_usage API response
#' @param response API response object
#' @return Parsed dataframe
#' @noRd
parse_match_response <- function(response) {
  data <- fromJSON(rawToChar(response$content))
  
  
  input <- as.data.frame(data['scientific name full'])
  colnames(input) <- "input_name"
  fields <- data[1:11]
  fields <-
    lapply(fields, function(x)
      if (is.null(x))
        NA
      else
        x)
  
  result <- cbind(input, fields)
  return(result)
}
