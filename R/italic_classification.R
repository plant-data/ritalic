#' @title Lichen classification
#' @description This function returns the classification of the lichen species passed as input.
#' @param sp_names A vector containing scientific names of lichens.
#' @return A dataframe containing the classification of the lichen species passed as input.
#' @examples
#' italic_classification(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#'
#' @export
italic_classification <-function(sp_names) {
  
  data <- call_api_base(sp_names, "https://italic.units.it/api/v1/classification/")
  return(data)
 
}
