#' @title Lichen match
#' @description This function aligns scientific names against the Checklist of the Lichens of Italy
#' @param sp_names A character vector of scientific names
#' @param subsp_marks An optional character vector of markers to match at the subspecies level
#' @param var_marks An optional character vector of markers to match at the variety level
#' @param form_marks An optional character vector of markers to match at the form level
#'
#' @return a list of matched names, with the original names as the names of the list and the matched names as the values
#' with seven columns, \code{input_name}, \code{matched_name}, 
#'.    \code{status}, \code{accepted_name}, \code{score}, \code{name_score}, \code{auth_score}
#'
#' @examples
#' lich_match(c("Cetraria islandica", "Lecanora ciliata"))
#'
#' @import utils
#' @import httr
#' @import jsonlite
#'
#' @export
lich_match <-function(sp_names, subsp_marks = c(), var_marks = c(), form_marks = c()) {

  # sp_names must be a vector
  if (!is.character(sp_names) && !is.vector(sp_names)) {
    stop("sp_string must be a string or a vector")
  } else if (is.character(sp_names)) {
    sp_names <- c(sp_names)
  }
  
  # replace Na with empty values
  sp_names <- ifelse(is.na(sp_names), "", sp_names)

  # create a vector with only unique species names
  unique_sp_names <- unique(sp_names)

  # for each unique name call the match api the result is put in a dataframe
  progress_bar <- define_progress_bar(length(unique_sp_names))
  for (i in 1:length(unique_sp_names)) {
    # these parameters are used to retry the iteration after a 429 code

    success <- FALSE
    while (!success) {
      
      sp_name <- unique_sp_names[i];
 
      url <- "https://italic.units.it/api/v2/match"
      headers <- c('Content-Type' = 'application/json')
      body <-
        list(
          'sp' = sp_name,
          'subsp-mark' = subsp_marks,
          'var-mark' = var_marks,
          'form-mark' = form_marks
        )

      # Send POST request to API
      response <-
        POST(url,
             body = jsonlite::toJSON(body),
             encode = "json",
             add_headers(headers))


      # Deal with api errors
      # 500 server not available (blocks the function)
      # 429 API usage limit exceeded
      if (response$status_code == 500) {
        stop("Impossible to connect to the server, please try again later")
      } else if (response$status_code == 429) {
        # wait the end of the api cooldown and retry
        wait_api_cooldown()
      } else if (response$status_code == 200) {
        success <- TRUE
      } else {
        stop("An unknown error occurred, please try again later")
      }

    }

    # If status_code = 200 everything is fine

    #progress bar

    # Extract response content
    data <-  fromJSON(rawToChar(response$content))


    input <-  as.data.frame(data[1])
    match <-  data[2]
    match <- lapply(match$match, function(x) if (is.null(x)) NA else x)
    result <- 
      cbind(input, match)
    if (i == 1) {
      result_merged <- result
    } else {
      result_merged <- rbind(result_merged, result)
    }

    utils::setTxtProgressBar(progress_bar, i)

  }
  print(sp_names)
  shs <- result_merged[1][1]
  hjgsdfghj <- result_merged[2][1]
  # at the end of the cycle, the original array is rebuilt
  ordered_dataframe <- reconstruct_order(sp_names, result_merged, 1)
  return(ordered_dataframe)

}
