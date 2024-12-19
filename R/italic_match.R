#' Match lichen scientific names against the database of ITALIC
#'
#' @description
#' Aligns scientific names of lichens against the Checklist of the Lichens of Italy available in ITALIC
#' database. The function handles infraspecific ranks (subspecies, varieties, forms) and
#' returns detailed matching information including nomenclatural status and matching scores.
#'
#' @param sp_names A character vector of scientific names to match
#' @param subsp_marks Character vector of markers used to indicate uncommon subspecies rank in the input names
#'        (different from "subsp.", "ssp."). For example, to match "Pseudevernia furfuracea b) ceratea",
#'        you need to pass "b)" as subsp_mark
#' @param var_marks Character vector of markers used to indicate uncommon variety rank in the input names
#'        (different from "var.", "v."). For example, to match "Acarospora sulphurata varietas rubescens",
#'        you need to pass "varietas" as var_mark
#' @param form_marks Character vector of markers used to indicate uncommon form rank in the input names
#'        (different from "f.", "form"). For example, to match "Verrucaria nigrescens fo. tectorum",
#'        you need to pass "fo." as form_mark
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{input_name}{Original scientific name provided}
#'     \item{matched_name}{Name matched in ITALIC database}
#'     \item{status}{Nomenclatural status ("accepted" or "synonym")}
#'     \item{accepted_name}{Currently accepted name in ITALIC}
#'     \item{name_score}{Matching score for the name part (0-100)}
#'     \item{auth_score}{Matching score for the authority part (0-100)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Simple name matching
#' result <- italic_match("Cetraria islandica")
#'
#' # Name matching with spelling mistakes
#' result <- italic_match("Xantoria parietina")
#'
#' # Matching with uncommon marker
#' result <- italic_match("Acarospora sulphurata varietas rubescens",
#'                       var_marks = "varietas")
#'
#' # Matching multiple names
#' result <- c("Cetraria islandica", "Xanthoria parietina")
#' }
#' @importFrom jsonlite fromJSON
#' @export
italic_match <-
  function(sp_names,
           subsp_marks = c(),
           var_marks = c(),
           form_marks = c()) {
    body <- list(
      'subsp-mark' = subsp_marks,
      'var-mark' = var_marks,
      'form-mark' = form_marks
    )
    
    data <-
      call_api_base(
        sp_names,
        api_endpoint = "https://italic.units.it/api/v1/match",
        loading_text = "Matching names ...",
        parse_function = parse_match_response,
        request_method = "POST",
        body = body,
        reorder_result = TRUE
      )
    return(data)
    
  }

#' Parse italic match API response
#' @param response API response object
#' @return Parsed dataframe
#' @noRd
parse_match_response <- function(response) {
  data <- fromJSON(rawToChar(response$content))
  
  
  input <- as.data.frame(data[1])
  match <- data[2]
  match <-
    lapply(match$match, function(x)
      if (is.null(x))
        NA
      else
        x)
  
  result <- cbind(input, match)
  return(result)
}