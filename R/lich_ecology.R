#' @title Lichen ecology
#' @description This function returns the ecology parameters of the lichen species passed as input. For more info about the ecology parameters see https://italic.units.it/?procedure=base&t=59&c=60#otherdata
#' @param sp_names A vector containing scientific names of lichens.
#' @return A dataframe containing the ecology of the lichen species passed as input.
#' 
#' @examples
#' lich_ecology(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#' @export
lich_ecology <-function(sp_names) {
  
  data <- call_api_base(sp_names, "https://italic.units.it/api/v1/ecology/")
  return(data)
  
}
