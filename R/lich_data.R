#' @title Lichen data
#' @description This function returns a dataframe containing the classification, description, ecology and rarity of the lichen species passed as input. For more info about these parameters see https://italic.units.it/?procedure=base&t=59&c=60#otherdata
#' @param sp_names A vector containing the scientific names of the lichen species.
#' @return A dataframe containing the classification, description, ecology and rarity of the lichen species passed as input.
#' @examples
#' lich_data(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#' @import utils
#'
#' @export
lich_data <- function(sp_names) {

  overview_bar = define_progress_bar(4)
  classification <- lich_classification(sp_names)
  utils::setTxtProgressBar(overview_bar, 1)
  description <- lich_description(sp_names)
  utils::setTxtProgressBar(overview_bar, 2)
  ecology <- lich_ecology(sp_names)
  utils::setTxtProgressBar(overview_bar, 3)
  rarity <- lich_rarity(sp_names)
  utils::setTxtProgressBar(overview_bar, 4)
  
  # in each dataset remove the first and last column
  classification2 <- classification[, 1:ncol(classification) - 1]
  description2 <- description[, 3:ncol(description) - 1]
  ecology2 <- ecology[, 3:ncol(ecology) - 1]
  rarity2 <- rarity[, 3:ncol(rarity) - 1]

  # merge the datasets with cbind
  result <- cbind(classification2, description2, ecology2, rarity2)

  return(result)
}
