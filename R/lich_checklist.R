#' @title Lichen checklist
#' @description This function returns the presence/absence (1 or 0 ) of the lichen species passed as input in each administrative region of Italy.
#' @param sp_names A vector containing scientific names of lichens.
#' @return A dataframe with the presence/absence of the lichen species passed as input in each administrative region of Italy
#' @examples
#' lich_checklist(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#'
#' @import httr
#' @import jsonlite
#'
#' @export
lich_checklist <- function(sp_names) {
  data <- call_api_base(sp_names, "https://italic.units.it/api/v1/checklist/")
  return(data)
}
