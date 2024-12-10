#' Get occurrence records for lichen taxa
#'
#' @description
#' Retrieves occurrence records from Italian herbarium collections for specified lichen taxa.
#' Only accepts names that exist in the database of ITALIC.
#'
#' @note Before using this function with a list of names, first obtain their accepted names
#'       using `italic_match()`.
#'       Example workflow:
#'       names_matched <- italic_match(your_names)
#'       occ <- italic_occurrences(names_matched$accepted_name)
#'
#' @param sp_names Character vector of accepted names
#' @param result_data Character string specifying output detail level:
#'        "simple" (default) or "extended"
#'
#' @return A data frame with occurrence records. Column names follow the Darwin Core standard, with the additional column substratum, which is particularly relevant for lichens. For simple output:
#'   \describe{
#'     \item{scientificName}{Full scientific name}
#'     \item{decimalLatitude}{Latitude in decimal degrees}
#'     \item{decimalLongitude}{Longitude in decimal degrees}
#'     \item{coordinatesUncertaintyInMeters}{Spatial uncertainty of the coordinates}
#'     \item{substratum}{Substrate on which the specimen was found}
#'     \item{institutionCode}{Code of the herbarium holding the specimen}
#'     \item{eventDate}{Collection date}
#'   }
#'
#'   Extended output adds:
#'   \describe{
#'     \item{locality}{Collection locality}
#'     \item{catalogNumber}{Specimen identifier in the collection}
#'     \item{minimumElevationInMeters}{Lower limit of the elevation range}
#'     \item{maximumElevationInMeters}{Upper limit of the elevation range}
#'     \item{verbatimIdentification}{Scientific name reported on the original label}
#'     \item{identifiedBy}{Person who identified the specimen}
#'   }
#'
#' @examples
#' \dontrun{
#' # Get simple occurrence data
#' occ <- italic_occurrences("Cetraria ericetorum Opiz")
#'
#' # Get extended occurrence data
#' occ_ext <- italic_occurrences("Cetraria ericetorum Opiz", result_data = "extended")
#' }
#'
#' @references
#' ITALIC - The Information System on Italian Lichens
#' \url{https://italic.units.it}
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @export
italic_occurrences <- function(sp_names, result_data = 'simple') {
  extra_params <-
    if (result_data == "extended")
      "&result_data=extended"
  else
    ""
  
  data <-
    call_api_base(
      sp_names,
      api_endpoint = "https://italic.units.it/api/v1/occurrences/",
      loading_text = "Retrieving occurrences...",
      parse_function = parse_occurrences_response,
      extra_param = extra_params,
      request_method = "GET",
      reorder_result = FALSE
    )
  
  return(data)
}

#' Parse occurrences API response
#' @param response API response object
#' @return Parsed dataframe or NULL if empty
#' @noRd
parse_occurrences_response <- function(response) {
  json_data <- fromJSON(rawToChar(response$content))
  
  input <- as.data.frame(json_data[1])
  data <- json_data[3]$data
  
  if (is.list(data) && length(data) == 0) {
    return(NULL)
  }
  
  result <- as.data.frame(data)
  return(result)
}