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
#'
#' @return A `ggplot` object representing the distribution map. The map displays Italian
#'         areas colored according to the commonness/rarity status of the specified
#'         lichen species.
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
#' }
#' 
#' @references
#' For more information about Italian ecoregions see ITALIC ecoregions distribution
#' \url{https://italic.units.it/?procedure=base&t=59&c=60#commonness}
#' and the scientific publication describing the ecoregions
#' \url{https://www.mdpi.com/1424-2818/12/8/294}
#' @importFrom sf read_sf
#' @export
italic_distribution_map <- function(sp_name) {
  
  geopackage_path <- system.file("extdata", "ecoregions.gpkg", package = "ritalic")
  
  data <- italic_ecoregions_distribution(sp_name)
  ecoregions <- suppressWarnings(sf::read_sf(geopackage_path))

  plot_rarity_map(ecoregions, data, sp_name)
}


#' plot the distribution map
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual theme_minimal ggtitle theme element_text element_rect element_blank
#' @importFrom stats setNames
#' @noRd
plot_rarity_map <- function(ecoregions, data, title_text) {
  
  # version using tidyr
  # lichen_data_long <- data %>%
  #   pivot_longer(
  #     cols = -scientific_name,
  #     names_to = "belt",
  #     values_to = "rarity"
  #   )
  
  # test alternative without external libraries
  lichen_data_long <- pivot_longer_lichen_distribution(data)
  
  
  # 2 join the reshaped data to the shapefile:
  ecoregions2 <-
    merge(ecoregions,
          lichen_data_long,
          by = "belt",
          all.x = TRUE)
  
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
  ecoregions2$rarity <-
    factor(ecoregions2$rarity, levels = names(rarity_colors))
  rarity <- ecoregions2$rarity
  
  ggplot() +
    geom_sf(
      data = ecoregions2,
      aes(fill = rarity),
      linewidth = 0.0001,
      show.legend = TRUE
    ) +
    scale_fill_manual(values = rarity_colors,
                      drop = FALSE,
                      # This is important to show all levels in the legend
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

#' utility function that replaces tidyr pivot_longer for this use case
#' @noRd
pivot_longer_lichen_distribution <- function(data) {
  value_columns <- setdiff(colnames(data), "scientific_name")
  
  result <- data.frame(
    scientific_name = rep(data$scientific_name, length(value_columns)),
    belt = rep(value_columns, each = nrow(data)),
    rarity = unlist(data[value_columns])
  )
  
  result <- result[order(result$scientific_name),]
  rownames(result) <- NULL
  
  return(result)
}
