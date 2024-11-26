#' @title Lichen distribution in ecoregions
#' @description This function returns the distribution in ecoregions of the lichen species passed as input. For more info about the function see https://italic.units.it/?procedure=base&t=59&c=60#commonness
#' @param sp_names A vector containing scientific names of lichens.
#' @param result_data Optional parameter specifying the type of data to return. Can be either "rarity" (default) or "presence-absence". If set to "presence-absence", the returned dataframe will contain binary values (0 for 'absent', 1 for present).
#' @return A dataframe containing the distribution in ecoregions of the lichen species passed as input.
#' @examples
#' italic_ecoregions_distribution(c("Cetraria ericetorum Opiz", "Lecanora ciliata"))
#'
#' @export
italic_ecoregions_distribution <-function(sp_names, result_data="rarity") {
  
  data <- call_api_base(sp_names, "https://italic.units.it/api/v1/rarity/")

  # Convert all columns except the first one to binary values if result_data == "presence-absence"
  if (result_data == "presence-absence") {
        for (col in names(data)[-1]) {
            data[[col]] <- ifelse(data[[col]] == "absent", 0, 1)
        }
    }

  return(data)
}
