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

  description <- italic_description(sp_names)

  data <- italic_taxon_data(sp_names)
  
  # in each dataset remove the first and last column
  taxonomy2 <- taxonomy[, 1:ncol(taxonomy) - 1]
  description2 <- description[, 3:ncol(description) - 1]
  data2 <- data[, 3:ncol(data) - 1]

  # merge the datasets with cbind
  result <- cbind(taxonomy2, description2, data2)

  return(result)
}
