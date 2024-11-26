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


  classification <- italic_taxonomy(sp_names)

  description <- italic_description(sp_names)

  ecology <- italic_taxon_data(sp_names)
  
  # in each dataset remove the first and last column
  classification2 <- classification[, 1:ncol(classification) - 1]
  description2 <- description[, 3:ncol(description) - 1]
  ecology2 <- ecology[, 3:ncol(ecology) - 1]

  # merge the datasets with cbind
  result <- cbind(classification2, description2, ecology2)

  return(result)
}
