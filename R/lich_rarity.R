#' @title Lichen rarity
#' @description This function returns the rarity of the lichen species passed as input. For more info about rarity see https://italic.units.it/?procedure=base&t=59&c=60#commonness
#' @param sp_names A vector containing scientific names of lichens.
#' @return A dataframe containing the rarity of the lichen species passed as input.
#' @examples
#' lich_rarity(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#'
#' @export
lich_rarity <-function(sp_names) {
  
  data <- call_api_base(sp_names, "https://italic.units.it/api/v1/rarity/")
  return(data)
  
}
