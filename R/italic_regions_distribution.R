#' @title Lichen distribution
#' @description This function returns the presence/absence (1 or 0 ) of the lichen species passed as input in each administrative region of Italy.
#' @param sp_names A vector containing scientific names of lichens.
#' @return A dataframe with the presence/absence of the lichen species passed as input in each administrative region of Italy
#' @examples
#' italic_distribution(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#'
#' @import httr
#' @import jsonlite
#'
#' @export
italic_distribution <- function(sp_names) {
  data <- call_api_base(sp_names, "https://italic.units.it/api/v1/distribution/")
  return(data)
}
