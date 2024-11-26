#' Make HTTP request with retry logic
#' @param method HTTP method ("GET" or "POST")
#' @param url API endpoint URL
#' @param body Request body (optional)
#' @param ... Additional arguments passed to httr functions
#' @return HTTP response object
#' @importFrom httr GET POST add_headers
make_request <- function(method, url, body = NULL, ...) {
  MAX_RETRIES <- 99
  retry_count <- 0
  
  while (retry_count < MAX_RETRIES) {
    tryCatch({
      response <- if (method == "GET") {
        GET(url, ...)
      } else {
        POST(url,
             body = if (!is.null(body)) jsonlite::toJSON(body) else NULL,
             encode = "json",
             add_headers('Content-Type' = 'application/json'),
             ...)
      }
      
      if (response$status_code == 200) {
        return(response)
      } else if (response$status_code == 429) {
        wait_api_cooldown()
        retry_count <- retry_count + 1
      } else {
        handle_api_error(response$status_code)
      }
      
    }, error = function(e) {
      retry_count <- retry_count + 1
      if (retry_count >= MAX_RETRIES) {
        stop(paste("Failed after", MAX_RETRIES, "attempts:", e$message))
      }
      Sys.sleep(1)
    })
  }
}

#' Handle API error responses
#' @param status_code HTTP status code
handle_api_error <- function(status_code) {
  if (status_code == 500) {
    stop("Server error - please try again later")
  } else {
    stop(paste("Request failed with status code:", status_code))
  }
}

#' Wait for API cooldown
#' NOTE: the rate limit is server-side, changing this value won't speed up the data retrival proccess
wait_api_cooldown <- function() {
  Sys.sleep(60)  # Wait 60 seconds for rate limit reset
}