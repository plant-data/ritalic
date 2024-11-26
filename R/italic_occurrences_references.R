#' Get references for occurrences datasets
#' @description Returns references of scientific publications about retrieved occurrences
#' @param occurrences_dataframe Dataframe from italic_occurrences()
#' @return Dataframe containing references and DOIs
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @export
italic_occurrences_references <- function(occurrences_dataframe) {
  # Validate input
  validate_occurrences_input(occurrences_dataframe)
  
  # Process herbaria codes
  herbaria <- process_herbaria_codes(occurrences_dataframe$institutionCode)
  
  # Make API request
  response <- make_request(
    method = "GET",
    url = construct_references_url(herbaria)
  )
  
  # Parse and return results
  references <- parse_references_response(response)
  return(references)
}

#' Validate occurrences dataframe input
#' @param df Input dataframe to validate
#' @return NULL, throws error if invalid
validate_occurrences_input <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  if (!"institutionCode" %in% names(df)) {
    stop("The dataframe must contain an 'institutionCode' column")
  }
  if (nrow(df) == 0) {
    stop("The dataframe is empty")
  }
}

#' Process herbaria codes from institution codes
#' @param institution_codes Vector of institution codes
#' @return Processed herbaria codes
process_herbaria_codes <- function(institution_codes) {
  # Get unique codes
  herbaria <- unique(institution_codes)
  
  # Remove 'herbarium ' prefix
  herbaria <- gsub("^herbarium ", "", herbaria, ignore.case = TRUE)
  
  # Remove empty or NA values
  herbaria <- herbaria[!is.na(herbaria) & nchar(herbaria) > 0]
  
  if (length(herbaria) == 0) {
    stop("No valid herbaria codes found")
  }
  
  return(herbaria)
}

#' Construct references API URL
#' @param herbaria Vector of herbaria codes
#' @return Constructed URL
construct_references_url <- function(herbaria) {
  base_url <- "https://italic.units.it/api/v1/references/"
  encoded_herbaria <- URLencode(paste0(herbaria, collapse = ";"), reserved = TRUE)
  paste0(base_url, encoded_herbaria)
}

#' Parse references API response
#' @param response API response object
#' @return Dataframe of references and DOIs
parse_references_response <- function(response) {
  # Parse JSON response
  content <- fromJSON(rawToChar(response$content))
  
  # Handle empty response
  if (length(content$references) == 0) {
    return(data.frame(
      reference = character(),
      doi = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Create references dataframe
  refs <- data.frame(
    reference = unlist(content$references$reference),
    doi = unlist(content$references$doi),
    stringsAsFactors = FALSE
  )
  
  # Remove rows with NA references
  refs <- refs[!is.na(refs$reference), ]
  
  # Reset row names
  row.names(refs) <- NULL
  
  return(refs)
}