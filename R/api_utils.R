#' Make HTTP request with retry logic
#' @param method HTTP method ("GET" or "POST")
#' @param url API endpoint URL
#' @param body Request body (optional)
#' @param ... Additional arguments passed to request functions
#' @return HTTP response object
#' @noRd
make_request <- function(method, url, body = NULL, ...) {
  MAX_RETRIES <- 5
  
  req <- httr2::request(url)
  
  if (method == "POST") {
    request_body <- if (is.null(body)) list() else body
    req <- httr2::req_headers(req, 'Content-Type' = 'application/json')
    req <- httr2::req_body_json(req, request_body)
  }
  
  # retry when the rate limit is reached (429) or the server is unavailable (503),
  # and when the request fails (e.g. network down); keep the error responses to
  # handle them with handle_api_error
  req <- httr2::req_retry(
    req,
    max_tries = MAX_RETRIES,
    retry_on_failure = TRUE,
    after = function(resp) api_cooldown_seconds(),
    backoff = function(tries) api_failure_wait_seconds()
  )
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  
  response <- tryCatch(
    httr2::req_perform(req),
    httr2_failure = function(e) {
      stop(paste("Failed after", MAX_RETRIES, "attempts:", conditionMessage(e)),
           call. = FALSE)
    }
  )
  
  status_code <- httr2::resp_status(response)
  if (status_code != 200) {
    handle_api_error(status_code)
  }
  return(response)
}

#' Handle API error responses
#' @param status_code HTTP status code
#' @noRd
handle_api_error <- function(status_code) {
  if (status_code == 500) {
    stop("Server error - please try again later")
  } else {
    stop(paste("Request failed with status code:", status_code))
  }
}

#' Parse API response for base function
#' Valid for most api in ITALIC
#' @param response API response object
#' @return Parsed dataframe
#' @noRd
parse_api_response <- function(response) {
  json_data <- fromJSON(httr2::resp_body_string(response))
  input <- as.data.frame(json_data[1])
  
  # for common api the data needed is in the third value
  data <- json_data[3]
  data <- lapply(data$data, function(x)
    if (is.null(x))
      NA
    else
      x)
  data <- as.data.frame(data)
  
  result <- cbind(input, data)
  return(result)
}

#' Seconds to wait for the API rate limit refresh
#' @description NOTE: the rate limit is server-side, changing this value won't speed up the data retrieval process
#' @noRd
api_cooldown_seconds <- function() {
  10  # wait 10 seconds for the soft rate limit reset
}

#' Seconds to wait before retrying a failed request (e.g. network error)
#' @noRd
api_failure_wait_seconds <- function() {
  1
}
