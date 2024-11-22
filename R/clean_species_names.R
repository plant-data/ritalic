#' Clean Species Names Input
#' @param sp_names Character vector containing scientific names
#' @return Cleaned and processed character vector
#' @noRd
clean_species_names <- function(sp_names) {
  if (!is.vector(sp_names, mode = "character")) {
    stop("sp_names must be a character vector", call. = FALSE)
  }
  replace(sp_names, is.na(sp_names), "")
  
  # Regular expression to match common Unicode invisible characters
  invisible_char_regex <- "[\u00AD\u034F\u200B-\u200F\u2028-\u202E\u2060-\u206F\uFEFF]"
  
  # Remove invisible characters using gsub
  cleaned_names <- gsub(invisible_char_regex, "", sp_names, perl = TRUE)

}