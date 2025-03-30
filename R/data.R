#' Ecoregions of Italy (GeoPackage Format)
#'
#' This dataset contains the ecoregions of Italy, represented as polygons in a GeoPackage file.
#' The data is used to show the distribution of lichen species within different ecoregions.
#'
#' @name ecoregions
#' @noRd
#'
#' @format A GeoPackage file (`.gpkg`) with 6 fields (attributes).
#' \describe{
#'   \item{OBJECTID}{The internal id of the polygon.}
#'   \item{ogu}{The code for the operational geographic unit.}
#'   \item{ecoregion}{The name of the ecoregion}
#'   \item{region}{The name of the administrative region}
#'   \item{geom}{The polygon shape}
#' }
#' Geometry type: MultiPolygon
#' Coordinate Reference System: WGS 84 (EPSG:4326)
#'
#' @source The original data was obtained from ITALIC 
#' and simplified with mapshaper to reduce the complexity of polygons. 
#'
#' @keywords datasets
NULL

#' Regions of Italy (GeoPackage Format)
#'
#' This dataset contains the administrative regions of Italy, represented as polygons in a GeoPackage file.
#' This data is used to show the distribution of lichen species within different regions.
#'
#' @name regions
#' @noRd
#'
#' @format A GeoPackage file (`.gpkg`) with the following fields (attributes):
#' \describe{
#'   \item{COD_REG}{The internal ID of the region polygon.}
#' }
#' Geometry type: MultiPolygon
#' Coordinate Reference System: WGS 84 (EPSG:4326)
#'
#' @source The original data was obtained from ITALIC 
#' and simplified with mapshaper to reduce the complexity of polygons. 
#'
#'
#' @keywords datasets
NULL