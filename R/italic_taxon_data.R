#' @title Get data of lichen taxa
#' @description This function returns a dataframe containing taxonomy, ecology_traits, regions_distribution, ecoregions_distribution of the lichen species passed as input. For more info about these parameters see https://italic.units.it/?procedure=base&t=59&c=60#otherdata
#' Only accepts names that exist in the database of ITALIC.
#'
#' @note Before using this function with a list of names, first obtain their accepted names
#'       using `italic_match()`.
#'       Example workflow:
#'       \preformatted{
#'       names_matched <- italic_match(your_names)
#'       italic_taxon_data(names_matched$accepted_name)
#'       }
#' @param sp_names A vector containing the scientific names of the lichen species.
#' @return A dataframe containing the taxonomy, ecology distribution and rarity of the lichen species passed as input.
#'
#' @examples
#' \dontrun{
#' italic_taxon_data(c("Cetraria ericetorum Opiz", "Lecanora salicicola H. Magn."))
#' }
#' 
#' @references
#' ITALIC - The Information System on Italian Lichens: data about taxa
#' \url{https://italic.units.it/?procedure=base&t=59&c=60#otherdata}
#'
#' @import utils
#' @export
italic_taxon_data <- function(sp_names) {
  taxonomy <- italic_taxonomy(sp_names)
  data <- italic_ecology_traits(sp_names)
  regions <- italic_regions_distribution(sp_names)
  ecoregions <- italic_ecoregions_distribution(sp_names)
  
  # in each dataframe remove the first and last column
  # scientific name is alwais the same and warning is not needed
  taxonomy2 <- taxonomy[, 1:ncol(taxonomy) - 1]
  data2 <- data[, 3:ncol(data) - 1]
  regions2 <- regions[, 3:ncol(regions) - 1]
  ecoregions2 <- ecoregions[, 3:ncol(ecoregions) - 1]
  
  result <- cbind(taxonomy2, data2, regions2, ecoregions2)
  
  return(result)
}
