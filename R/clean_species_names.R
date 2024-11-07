#' Clean Species Names Input
#' @param sp_names Character vector containing scientific names
#' @return Cleaned and processed character vector
#' @noRd
clean_species_names <- function(sp_names) {
  if (!is.vector(sp_names, mode = "character")) {
    stop("sp_names must be a character vector", call. = FALSE)
  }
  replace(sp_names, is.na(sp_names), "")
}