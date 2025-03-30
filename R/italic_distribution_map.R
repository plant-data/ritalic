#' Create distribution map of a lichen taxon
#' 
#' @description
#' Creates a distribution map for a given lichen species based on its commonness/rarity
#' status across Italian ecoregions and presence/absence across administrative regions. 
#' The map visually represents the data obtained from
#' `italic_ecoregions_distribution()` and `italic_regions_distribution()`.
#' 
#' @note Before using this function, ensure that you have obtained the accepted name of the
#'       lichen using `italic_match()`.
#'       Example workflow:
#'       \preformatted{
#'       name_matched <- italic_match("Cetraria islandica")
#'       map <- italic_distribution_map(name_matched$accepted_name)
#'       }
#'       
#' @param sp_name Character string representing the accepted scientific name of a lichen
#'                species.
#' @param plot_map If TRUE (default) the function returns a ggplot graph, if FALSE returns a sf object 
#'                
#' @return if plot_map = TRUE (default) a `ggplot` object representing the distribution map where Italian areas are colored according to the species' commonness/rarity. If plot_map = FALSE the sf object used to create the plot
#'         
#' @details
#' The function internally utilizes `italic_ecoregions_distribution()` and `italic_regions_distribution()` to retrieve the commonness/rarity
#' status across Italian ecoregions and presence/absence across administrative regions data for the provided species. 
#' It then joins this data with a geospatial dataset of Italian regions and ecoregions (included in the package) to generate the map.
#' 
#' Commonness/rarity categories are visualized with a color scale, where each color
#' corresponds to a different level of commonness/rarity ("extremely common", "very common",
#' "common", "rather common", "rather rare", "rare", "very rare", "extremely rare", "absent").
#'
#' @examples
#' \dontrun{
#' italic_distribution_map("Flavoparmelia caperata (L.) Hale")
#' italic_distribution_map("Anisomeridium biforme (Schaer.) R.C. Harris")
#' }
#' 
#' @references
#' For more information about Italian ecoregions see ITALIC ecoregions distribution
#' \url{https://italic.units.it/?procedure=base&t=59&c=60#commonness}
#' and the scientific publication describing the ecoregions used in ITALIC
#' \url{https://www.mdpi.com/1424-2818/12/8/294}
#' @importFrom sf read_sf
#' @export
italic_distribution_map <- function(sp_name, plot_map=TRUE) {
  
  if (!is.atomic(sp_name) || length(sp_name) != 1){
    stop("Only one name allowed")
  }
  ecoregions_path <- system.file("extdata", "ecoregions.gpkg", package = "ritalic")
  regions_path <- system.file("extdata", "regions.gpkg", package = "ritalic")
  
  ecoregions_distribution <- italic_ecoregions_distribution(sp_name)
  regions_distribution <- italic_regions_distribution(sp_name)
  base_map <- sf::read_sf(ecoregions_path)
  regions_map <- sf::read_sf(regions_path)
  
  plot_rarity_map(base_map, regions_map, ecoregions_distribution, regions_distribution, sp_name, plot_map)
  
}


#' plot the distribution map
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual theme_minimal ggtitle theme element_text element_rect element_blank
#' @importFrom stats setNames
#' @noRd
plot_rarity_map <- function(base_map, regions_map, ecoregions_distribution, regions_distribution, title_text, plot_map) {
  
  # version using tidyr
  # lichen_data_long <- data %>%
  #   pivot_longer(
  #     cols = -scientific_name,
  #     names_to = "belt",
  #     values_to = "rarity"
  #   )
  
  
  # test alternative without external libraries
  ecoregions_distribution_long <- pivot_longer_ecoregions(ecoregions_distribution)
  regions_distribution_long <- pivot_longer_regions(regions_distribution)
  if (is.na(ecoregions_distribution_long$rarity[1])) {
    ecoregions_distribution_long$rarity = 0
    regions_distribution_long$presence = 0
    warning("Name not in ITALIC")
  }
  regions_distribution_long <- regions_distribution_long[,-1]
  # join the reshaped data to the shapefile:
  base_map <-
    merge(base_map,
          ecoregions_distribution_long,
          by = "ecoregion",
          all.x = TRUE)
  
  base_map <-
    merge(base_map,
          regions_distribution_long,
          by = "region",
          all.x = TRUE)
  
  base_map$rarity[base_map$presence == 0] <- "absent"
  
  
  # 3 create the blue color scale of italic:
  blue_scale <- c(
    "absent" = "white",
    "extremely rare" = "#bcddfb",
    "very rare" = "#7dbdfd",
    "rare" = "#3c9efc",
    "rather rare" = "#0482fc",
    "rather common" = "#0961db",
    "common" = "#0442b3",
    "very common" = "#03228b",
    "extremely common" = "#030264"
  )
  
  rarity_colors <- setNames(
    blue_scale,
    c(
      "absent",
      "extremely rare",
      "very rare",
      "rare",
      "rather rare",
      "rather common",
      "common",
      "very common",
      "extremely common"
    )
  )
  
  # hacky way to display all rarity levels in the legend
  base_map$rarity <-
    factor(base_map$rarity, levels = names(rarity_colors))
  rarity <- base_map$rarity
  
  if (!plot_map) {
    return(base_map)
  }
  
  ggplot() +
    geom_sf(
      data = base_map,
      aes(fill = rarity),
      lwd = 00000000000000000000.1,
      color = NA,
      show.legend = TRUE
    ) +
    geom_sf(data = regions_map, fill = NA, color = "black", lwd = 0.1) + 
    scale_fill_manual(values = rarity_colors,
                      drop = FALSE,
                      # this is important to show all levels in the legend
                      name = "Rarity") +
    theme_minimal() +
    ggtitle(title_text) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        vjust = 2,
        face = "bold",
        size = 12
      ),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.1
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

}

#' utility function that replaces tidyr pivot_longer for ecoregions
#' @noRd
pivot_longer_ecoregions <- function(data) {
  value_columns <- setdiff(colnames(data), "scientific_name")
  
  result <- data.frame(
    scientific_name = rep(data$scientific_name, length(value_columns)),
    ecoregion = rep(value_columns, each = nrow(data)),
    rarity = unlist(data[value_columns])
  )
  
  result <- result[order(result$scientific_name),]
  rownames(result) <- NULL
  
  return(result)
}

#' utility function that replaces tidyr pivot_longer for regions
#' @noRd
pivot_longer_regions <- function(data) {
  value_columns <- setdiff(colnames(data), "scientific_name")
  
  result <- data.frame(
    scientific_name = rep(data$scientific_name, length(value_columns)),
    region = rep(value_columns, each = nrow(data)),
    presence = unlist(data[value_columns])
  )
  
  result <- result[order(result$scientific_name),]
  rownames(result) <- NULL
  
  return(result)
}
