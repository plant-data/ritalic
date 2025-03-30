#' @title Get a presence-absence matrix of lichen traits
#' 
#' @description This function returns morphological traits of the lichen species passed as input.
#' Only accepts names that exist in the database of ITALIC.
#'
#' @note Before using this function with a list of names, first obtain their accepted names
#'       using `italic_match()`.
#'       Example workflow:
#'       \preformatted{
#'       names_matched <- italic_match(your_names)
#'       traits <- italic_taits_pa(names_matched$accepted_name)
#'       }
#' @param sp_names A vector containing scientific names of lichens.
#' @return A dataframe containing a series of traits for the lichen species passed as input.
#'
#' @examples
#' \dontrun{
#' italic_traits_pa("Cetraria ericetorum Opiz")
#' }
#' @export
italic_traits_pa <- function(sp_names) {
  data <-
    call_api_base(
      sp_names,
      "https://italic.units.it/api/v1/traits-pa/",
      "Retrieving pa matrix of traits...",
      parse_function = parse_api_response,
      request_method = "GET",
      reorder_result = TRUE
    )
  return(data)
  
}
