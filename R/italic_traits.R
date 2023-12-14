#' @title Lichen traits
#' @description This function returns the functional traits of the lichen species passed as input.
#' @param sp_names A vector containing scientific names of lichens.
#' @return A dataframe containing the ecology of the lichen species passed as input.
#' 
#' @examples
#'italic_traits(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#' 
#' @export
italic_traits <-function(sp_names) {
  
  data <- call_api_base(sp_names, "https://italic.units.it/api/v1/traits/")
  return(data)
  
}
