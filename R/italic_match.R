#' Match Lichen Scientific Names Against the Database of ITALIC
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
    
    # prepare and validate input
    sp_names <- prepare_species_names(sp_names)
    unique_sp_names <- unique(sp_names)
    

    pb <-
      create_progress_bar(length(unique_sp_names), "Processing name match...")
    
    results_list <- vector("list", length(unique_sp_names))
    
    # match each species
    for (i in seq_along(unique_sp_names)) {
      body <- list(
        'sp' = unique_sp_names[i],
        'subsp-mark' = subsp_marks,
        'var-mark' = var_marks,
        'form-mark' = form_marks
      )
      
      response <- make_request(method = "POST",
                               url = "https://italic.units.it/api/v1/match",
                               body = body)
      
      results_list[[i]] <- parse_match_response(response)
      update_progress(pb, i)
    }
    
 
    close_progress_bar(pb)
    
    result_merged <- do.call(rbind, results_list)
    row.names(result_merged) <- NULL  # Reset row names
    
    # restore original order
    ordered_dataframe <- reconstruct_order(sp_names, result_merged, 1)
    
    return(ordered_dataframe)
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