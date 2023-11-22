#' @title Lichen description
#' @description This function returns the description of the lichen species passed as input.
#' @param sp_names A vector containing scientific names of lichens.
#' @return A dataframe containing the description of the lichen species passed as input.
#' 
#' @examples
#' lich_description(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#'
#' @export
lich_description <-function(sp_names) {
  
  data <- call_api_base(sp_names, "https://italic.units.it/api/v1/description/")
  return(data)

}
