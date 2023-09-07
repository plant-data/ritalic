# Write the full documentation
#' @title Lichen data
#' @description This function returns a dataframe containing the classification, description, ecology and rarity of the lichen species passed as input.
#' @param sp_names A vector containing the scientific names of the lichen species.
#' @return A dataframe containing the classification, description, ecology and rarity of the lichen species passed as input.
#' @examples
#' lich_data(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#' @import utils
#' @import httr
#' @import jsonlite
#'
#' @export
lich_data <- function(sp_names) {
  classification <- lich_classification(sp_names)
  description <- lich_description(sp_names)
  ecology <- lich_ecology(sp_names)
  rarity <- lich_rarity(sp_names)
  
  # in each dataset remove the first and last column
  classification <- classification[, 1:ncol(classification) - 1]
  description <- description[, 2:ncol(description) - 1]
  ecology <- ecology[, 2:ncol(ecology) - 1]
  rarity <- rarity[, 2:ncol(rarity) - 1]

  # merge the datasets with cbind
  result <- cbind(classification, description, ecology, rarity)

  return(result)
}
