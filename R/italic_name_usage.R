#' Get details of species names
#'
#' @description
#' Retrieves information for a scientific name used in ITALIC, including name id,
#' taxonomic status, Index Fungorum id and related taxon id.
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
#' @param sp_names Character vector of matched names or accepted names
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{input_name}{The scientific name provided as input}
#'     \item{scientific_name_id}{Unique identifier of ITALIC for the scientific name}
#'     \item{index_fungorum_id}{Corresponding Index Fungorum identifier}
#'     \item{scientific_name_full}{Complete scientific name including authority}
#'     \item{scientific_name}{Scientific name without authority}
#'     \item{authorship}{Author of the name}
#'     \item{notes}{Additional notes about the taxon, if any}
#'     \item{rank}{Taxonomic rank of the name}
#'     \item{status}{Taxonomic status ('accepted', 'synonym' or 'basionym')}
#'     \item{related_accepted_name_id}{ID of the currently accepted name related to the input name in ITALIC}
#'     \item{related_accepted_name}{Full accepted name}
#'     \item{related_taxon_id}{ID of the related taxon in ITALIC}
#'   }
#'
#' @examples
#' \dontrun{
#' italic_name_usage(c("Cetraria islandica (L.) Ach. subsp. islandica", "Secoliga annexa Arnold"))
#' }
#'
#' @export
italic_name_usage <- function(sp_names) {
  data <-
    call_api_base(
      sp_names,
      api_endpoint = "https://italic.units.it/api/v1/name-usage/",
      loading_text = "Retrieving name data...",
      parse_function = parse_name_usage_response,
      request_method = "GET",
      reorder_result = TRUE
    )
  return(data)
  
}

#' Parse italic name_usage API response
#' @param response API response object
#' @return Parsed dataframe
#' @noRd
parse_name_usage_response <- function(response) {
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
