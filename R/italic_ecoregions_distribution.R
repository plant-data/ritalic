#' Get distribution of lichen taxa across Italian ecoregions
#'
#' @description
#' Returns the distribution and commonness status of lichen taxa across Italian
#' ecoregions. Only accepts names that exist in the database of ITALIC.
#'
#' @note Before using this function with a list of names, first obtain their accepted names
#'       using `italic_match()`.
#'       Example workflow:
#'       names_matched <- italic_match(your_names)
#'       ecoregions_distribution <- italic_ecoregions_distribution(names_matched$accepted_name)
#'
#' @param sp_names Character vector of accepted names
#' @param result_data Character string specifying the output format: "rarity" (default) returns
#'        commonness/rarity categories, "presence-absence" returns only values for presence/absence (0/1)
#'
#' @return A data frame with:
#'   \describe{
#'     \item{scientific_name}{Scientific name with authorities}
#'     \item{alpine}{Status in alpine belt (extremely common to absent)}
#'     \item{subalpine}{Status in subalpine belt (extremely common to absent)}
#'     \item{oromediterranean}{Status in oromediterranean belt (extremely common to absent)}
#'     \item{montane}{Status in montane belt (extremely common to absent)}
#'     \item{dry_submediterranean}{Status in dry submediterranean belt (extremely common to absent)}
#'     \item{padanian}{Status in padanian belt (extremely common to absent)}
#'     \item{humid_submediterranean}{Status in humid submediterranean belt (extremely common to absent)}
#'     \item{humid_mediterranean}{Status in humid mediterranean belt (extremely common to absent)}
#'     \item{dry_mediterranean}{Status in dry mediterranean belt (extremely common to absent)}
#'   }
#'   The possible values of commonness/rarity are: "extremely common", "very common", "common", "rather common",
#'   "rather rare", "rare", "very rare", "extremely rare", "absent"
#'
#' @examples
#' \dontrun{
#' # Get commonness/rarity categories
#' ecodist <- italic_ecoregions_distribution("Cetraria ericetorum Opiz")
#'
#' # Get presence/absence data
#' edist <- italic_ecoregions_distribution("Cetraria ericetorum Opiz", "presence-absence")
#' }
#'
#' @references
#' ITALIC - The Information System on Italian Lichens: ecoregions distribution
#' \url{https://italic.units.it/?procedure=base&t=59&c=60#commonness}
#'
#' @export
italic_ecoregions_distribution <-
  function(sp_names, result_data = "rarity") {
    data <-
      call_api_base(
        sp_names,
        "https://italic.units.it/api/v1/ecoregions-distribution/",
        "Retrieving distribution in ecoregions...",
        parse_function = parse_api_response,
        request_method = "GET",
        reorder_result = TRUE
      )
    
    # convert all columns except the first one to binary values if result_data == "presence-absence"
    if (result_data == "presence-absence") {
      for (col in names(data)[-1]) {
        data[[col]] <- ifelse(data[[col]] == "absent", 0, 1)
      }
    }
    
    return(data)
  }
