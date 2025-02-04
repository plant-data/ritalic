#' Ecoregions of Italy (GeoPackage Format)
#'
#' This dataset contains the ecoregions of Italy, represented as polygons in a GeoPackage file.
#' The data is used to analyze the distribution of lichen species within different ecoregions.
#'
#' @name ecoregions_italy
#' @docType data
#'
#' @format A GeoPackage file (`.gpkg`) with 6 fields (attributes).
#' \describe{
#'   \item{OBJECTID}{Number. The internal id of the polygon.}
#'   \item{ogu}{Number. The code for the operational geographic unit.}
#'   \item{fascia}{Number. The code of the ecoregion}
#'   \item{ecoregion}{Character. The name of the ecoregion}
#'   \item{region}{Character. The name of the administrative region}
#'   \item{geom}{MultyPolygon. The polygon shape}
#' }
#' Geometry type: MultiPolygon
#' Coordinate Reference System: WGS 84 (EPSG:4326)
#'
#' @source The original data was obtained from ITALIC 
#' and simplified with sf: to reduce the complexity of polygons. 
#' Data source for the full resolution geopakage: \url{https://example.com/TODO}. 
#' License: CC BY 4.0
#'
#' @examples
#' # Access the file path using system.file()
#' ecoregions_path <- system.file("extdata", "ecoregions.gpkg", package = "ritalic")
#'
#'
#' @keywords datasets
NULL