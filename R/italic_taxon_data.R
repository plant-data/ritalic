#' @title Get data of lichen taxa
#' @description This function returns a dataframe containing taxonomy, ecology_traits, regions_distribution, ecoregions_distribution of the lichen species passed as input. For more info about these parameters see https://italic.units.it/?procedure=base&t=59&c=60#otherdata
#' Only accepts names that exist in the database of ITALIC.
#'
#' @note Before using this function with a list of names, first obtain their accepted names
#'       using `italic_match()`.
#'       Example workflow:
#'       \preformatted{
#'       names_matched <- italic_match(your_names)
#'       italic_taxon_data(names_matched$accepted_name)
#'       }
#' @param sp_names A vector containing the scientific names of the lichen species.
#' @return A data frame with one row for each name in `sp_names` (in the same order) and the columns:
#'   \describe{
#'     \item{scientific_name}{The scientific name provided as input}
#'     \item{phylum, class, order, family, genus}{Taxonomy, see [italic_taxonomy()]}
#'     \item{substrata, ..., poleotolerance_max}{Ecology and traits, see [italic_ecology_traits()]}
#'     \item{abruzzo, ..., veneto}{Presence (1) or absence (0) in the Italian administrative regions,
#'       see [italic_regions_distribution()]}
#'     \item{alpine, ..., dry_mediterranean}{Commonness/rarity in the Italian ecoregions,
#'       see [italic_ecoregions_distribution()]}
#'   }
#'
#' @examples
#' \dontrun{
#' italic_taxon_data(c("Cetraria ericetorum Opiz", "Lecanora salicicola H. Magn."))
#' }
#' 
#' @references
#' ITALIC - The Information System on Italian Lichens: data about taxa
#' \url{https://italic.units.it/?procedure=base&t=59&c=60#otherdata}
#'
#' @import utils
#' @export
italic_taxon_data <- function(sp_names) {
  taxonomy <- italic_taxonomy(sp_names)
  data <- italic_ecology_traits(sp_names)
  regions <- italic_regions_distribution(sp_names)
  ecoregions <- italic_ecoregions_distribution(sp_names)
  
  # the first column of each dataframe is the scientific name:
  # keep it only from the taxonomy
  data2 <- data[, -1, drop = FALSE]
  regions2 <- regions[, -1, drop = FALSE]
  ecoregions2 <- ecoregions[, -1, drop = FALSE]
  
  result <- cbind(taxonomy, data2, regions2, ecoregions2)
  
  return(result)
}
