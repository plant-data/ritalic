#' Get descriptions of lichen taxa
#'
#' @description
#' Retrieves the morphological description and dditional taxonomic or ecological notes about lichen taxa present in the Checklist of the Lichens of Italy.
#' Only accepts names that exist in the database of ITALIC.
#'
#' @note Before using this function with a list of names, first obtain their accepted names
#'       using `italic_match()`.
#'       Example workflow:
#'       names_matched <- italic_match(your_names)
#'       descriptions <- italic_description(names_matched$accepted_name)
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
#' italic_description("Cetraria islandica (L.) Ach. subsp. islandica")
#' }
#'
#'
#' @export
italic_description <- function(sp_names) {
  data <-
    call_api_base(
      sp_names,
      api_endpoint = "https://italic.units.it/api/v1/description/",
      loading_text = "Retrieving descriptions...",
      parse_function = parse_api_response,
      request_method = "GET",
      reorder_result = TRUE
    )
  return(data)
  
}
