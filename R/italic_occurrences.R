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
#' @return A data frame with occurrence records. For simple output:
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
#'   }
#'
#' @examples
#' \dontrun{
#' # Get simple occurrence data
#' occ <- italic_occurrences("Cetraria islandica")
#'
#' # Get extended occurrence data
#' occ_ext <- italic_occurrences("Cetraria islandica", result_data = "extended")
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

  sp_names <- prepare_species_names(sp_names)
  unique_sp_names <- unique(sp_names)
  
  pb <- create_progress_bar(
    length(unique_sp_names), 
    "Retrieving occurrences..."
  )
  
  results_list <- vector("list", length(unique_sp_names))
  has_results <- FALSE
  
  # call API for each species
  for (i in seq_along(unique_sp_names)) {

    sp_name <- URLencode(unique_sp_names[i], reserved = TRUE)
    url <- paste0(
      "https://italic.units.it/api/v1/occurrences/",
      sp_name,
      if(result_data == 'extended') '&result_data=extended' else ''
    )
    
    response <- make_request(
      method = "GET",
      url = url
    )
    
    result <- parse_occurrences_response(response)
    
    if (!is.null(result) && nrow(result) > 0) {
      results_list[[i]] <- result
      has_results <- TRUE
    }
    
    update_progress(pb, i)
  }
  
  close_progress_bar(pb)
  
  if (!has_results) {
    return(data.frame())  # Return empty dataframe if no results
  }
  
  valid_results <- Filter(Negate(is.null), results_list)
  result_merged <- do.call(rbind, valid_results)
  row.names(result_merged) <- NULL  # Reset row names
  
  return(result_merged)
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