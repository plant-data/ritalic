#' Get scientific references for occurrence data
#'
#' @description
#' Retrieves bibliographic references and DOIs for scientific publications describing
#' occurrence datasets from specific herbarium collections.
#'
#' @param occurrences_dataframe Data frame containing occurrence records, must include
#'        an 'institutionCode' column
#'
#' @return A data frame with two columns:
#'   \describe{
#'     \item{reference}{Full bibliographic citation of the publication}
#'     \item{doi}{Digital Object Identifier URL}
#'   }
#'
#' @examples
#' \dontrun{
#' # Get occurrences first
#' occ <- italic_occurrences("Cetraria ericetorum Opiz")
#'
#' # Then get associated references
#' refs <- italic_occurrences_references(occ)
#' }
#'
#' @export
italic_occurrences_references <- function(occurrences_dataframe) {
  validate_occurrences_input(occurrences_dataframe)
  
  herbaria <-
    process_herbaria_codes(occurrences_dataframe$institutionCode)
  
  response <- make_request(method = "GET",
                           url = construct_references_url(herbaria))
  references <- parse_references_response(response)
  return(references)
}

#' Validate occurrences dataframe input
#' @param df Input dataframe to validate
#' @return NULL, throws error if invalid
#' @noRd
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
#' @noRd
process_herbaria_codes <- function(institution_codes) {
  herbaria <- unique(institution_codes)
  herbaria <- gsub("^herbarium ", "", herbaria, ignore.case = TRUE)
  herbaria <- herbaria[!is.na(herbaria) & nchar(herbaria) > 0]
  
  if (length(herbaria) == 0) {
    stop("No valid herbaria codes found")
  }
  
  return(herbaria)
}

#' Construct references API URL
#' @param herbaria Vector of herbaria codes
#' @return Constructed URL
#' @noRd
construct_references_url <- function(herbaria) {
  base_url <- "https://italic.units.it/api/v1/references/"
  encoded_herbaria <-
    URLencode(paste0(herbaria, collapse = ";"), reserved = TRUE)
  paste0(base_url, encoded_herbaria)
}

#' Parse references API response
#' @param response API response object
#' @return Dataframe of references and DOIs
#' @noRd
parse_references_response <- function(response) {
  content <- fromJSON(rawToChar(response$content))
  
  if (length(content$references) == 0) {
    return(data.frame(
      reference = character(),
      doi = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  refs <- data.frame(
    reference = unlist(content$references$reference),
    doi = unlist(content$references$doi),
    stringsAsFactors = FALSE
  )
  
  
  refs <- refs[!is.na(refs$reference),]
  row.names(refs) <- NULL
  
  return(refs)
}