#' Get taxonomic classification of lichen taxa
#'
#' @description
#' Retrieves the complete taxonomic hierarchy for lichen taxa from the ITALIC database.
#' Only accepts names that exist in the database of ITALIC.
#'
#' @note Before using this function with a list of names, first obtain their accepted names
#'       using `italic_match()`.
#'       Example workflow:
#'       names_matched <- italic_match(your_names)
#'       taxonomy <- italic_taxonomy(names_matched$accepted_name)
#'
#' @param sp_names Character vector of accepted names
#'
#' @return A data frame with:
#'   \describe{
#'     \item{scientific_name}{Scientific name}
#'     \item{phylum}{Phylum}
#'     \item{class}{Class}
#'     \item{order}{Order}
#'     \item{family}{Family}
#'     \item{genus}{Genus}
#'   }
#'
#' @examples
#' \dontrun{
#' taxonomy <- italic_taxonomy("Cetraria islandica (L.) Ach. subsp. islandica")
#' }
#'
#' @export
italic_taxonomy <- function(sp_names) {
  data <-
    call_api_base(
      sp_names,
      "https://italic.units.it/api/v1/taxonomy/",
      "Retrieving taxonomic classification...",
      parse_function = parse_api_response,
      request_method = "GET",
      reorder_result = TRUE
    )
  return(data)
  
}