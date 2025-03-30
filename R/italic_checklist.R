#' Get species names in the checklist of the lichens of Italy
#'
#' @description
#' Retrieves the complete list of accepted scientific names from the Checklist of
#' the Lichens of Italy in ITALIC. The function returns all accepted names of species occurring in Italy.
#' If the parameter include_bordering_countries is set to TRUE the function returns all the accepted names of species in ITALIC occurring both in Italy and in bordering countries.
#' @param include_bordering_countries Optional. Default FALSE. If TRUE the result includes also taxa occurring in bordering countries.
#' @param genus Optional. A genus name to filter the checklist.
#' @param family Optional. A family name to filter the checklist.
#' @param order Optional. An order name to filter the checklist.
#' @param class Optional. A class name to filter the checklist.
#' @param phylum Optional. A phylum name to filter the checklist.
#'
#' @return A character vector containing all accepted scientific names from the checklist of ITALIC.
#'
#' @examples
#' \dontrun{
#' # Get the complete checklist of Italy
#' italic_checklist()
#' # Get the complete checklist of Italy and bordering countries
#' italic_checklist(include_bordering_countries=TRUE)
#' # Get the checklist of the species of genus Lecanora
#' italic_checklist(genus ="Lecanora")
#' }
#'
#' @references
#' ITALIC - The Information System on Italian Lichens: checklist
#' \url{https://italic.units.it/index.php?procedure=checklist}
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
italic_checklist <-
  function(include_bordering_countries = FALSE,
           genus = NULL,
           family = NULL,
           order = NULL,
           class = NULL,
           phylum = NULL) {
    url <- "https://italic.units.it/api/v2/checklist/"
    
    params <- list()
    
    if (include_bordering_countries == TRUE)
      params$`include-bordering-countries`  <- URLencode('true', reserved = TRUE)
    if (!is.null(genus))
      params$genus <- URLencode(genus, reserved = TRUE)
    if (!is.null(family))
      params$family <- URLencode(family, reserved = TRUE)
    if (!is.null(order))
      params$order <- URLencode(order, reserved = TRUE)
    if (!is.null(class))
      params$class <- URLencode(class, reserved = TRUE)
    if (!is.null(phylum))
      params$phylum <- URLencode(phylum, reserved = TRUE)
    
    
    
    if (length(params) > 0) {
      query_string <-
        paste(names(params),
              params,
              sep = "=",
              collapse = "&")
      url <- paste0(url, "?", query_string)
    }
    response <- GET(url)
    
    if (response$status_code == 500) {
      stop("Impossible to connect to the server, please try again later")
    } else if (response$status_code == 429) {
      stop("Rate limit reached, please try again later")
    } else if (response$status_code == 200) {
      success <- TRUE
    } else {
      stop("An unknown error occurred, please try again later")
    }
    
    data <- fromJSON(rawToChar(response$content))
    
    checklist <- data[1]
    checklist <- checklist$checklist
    
    return(checklist)
    
  }