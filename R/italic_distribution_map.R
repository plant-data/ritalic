# get the distribution map of a lichen
#shapefile_path <- system.file("extdata", "your_shapefile.shp", package = "yourpackage") # For shapefile
#geopackage_path <- system.file("extdata", "your_shapefile.gpkg", package = "yourpackage") # For GeoPackage

#if (shapefile_path != "") { # Check if file exists (important for testing)
#  my_shapefile <- sf::st_read(shapefile_path)
#  # ... use my_shapefile in your package functions ...
#} else {
#  stop("Shapefile not found in package installation.")
#}