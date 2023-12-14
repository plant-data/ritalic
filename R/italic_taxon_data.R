#' @title Taxon data
#' @description This function returns morpho-functional traits, ecological indicators, altitudinal distribution and poleotolerance of the species passed as input. For more info about the returned parameters see https://italic.units.it/?procedure=base&t=59&c=60#otherdata
#' @param sp_names A vector containing scientific names of lichens.
#' @return A dataframe containing the ecology of the lichen species passed as input.
#' 
#' @examples
#' italic_taxon_data(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#' @export
italic_taxon_data <-function(sp_names) {
  
  data <- call_api_base(sp_names, "https://italic.units.it/api/v1/data/")
  return(data)
  
}
