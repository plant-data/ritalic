#' @title References for the occurrences datasets
#' @description This function returns the references of the scientific publications about the retrieved occurrences from italic_occurrences()
#' @param occurrences_dataframe The dataframe resulting from italic_occurrences()
#' @return A dataframe containing the references and the doi of the scientific publications of the retrieved occurrences
#'
#' @import httr
#' @import jsonlite
#'
#' @export
italic_occurrences_references <- function(occurrences_dataframe) {
  # Input validation
  if (!"institutionCode" %in% names(occurrences_dataframe)) {
    stop("The dataframe must contain an 'institutionCode' column")
  }
  
  # Get unique institution codes and remove 'herbarium ' prefix
  herbaria <- unique(occurrences_dataframe$institutionCode)
  herbaria <- gsub("^herbarium ", "", herbaria, ignore.case = TRUE)
  
  # Prepare the API URL with herbaria codes
  base_url <- "https://italic.units.it/api/v1/references/"
  url <- paste0(base_url, URLencode(paste0(herbaria, collapse = ";")), reserved = TRUE)
  # Make API request and handle response
  response <- GET(url)
  
  if (status_code(response) == 200) {
    # Parse JSON response
    content <- fromJSON(rawToChar(response$content))
    
    
    if (length(content$references) > 0) {
      data <- data.frame(
        reference = unlist(content$references$reference),
        doi = unlist(content$references$doi),
        stringsAsFactors = FALSE
      )
      data <- data[!is.na(data$reference), ]
      return(data)
    } else {
      return(data.frame(
        reference = character(),
        doi = character(),
        stringsAsFactors = FALSE
      ))
    }
  } else {
    stop("Impossible to retrieve data")
  }
}