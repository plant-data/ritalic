#' @title Lichen data
#' @description This function returns a dataframe containing the classification, description, taxon_data and rarity of the lichen species passed as input. For more info about these parameters see https://italic.units.it/?procedure=base&t=59&c=60#otherdata
#' @param sp_names A vector containing the scientific names of the lichen species.
#' @return A dataframe containing the classification, description, ecology and rarity of the lichen species passed as input.
#' @examples
#' italic_taxon_data_all(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#' @import utils
#'
#' @export
italic_taxon_data_all <- function(sp_names) {


  taxonomy <- italic_taxonomy(sp_names)
  data <- italic_taxon_data(sp_names)
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
