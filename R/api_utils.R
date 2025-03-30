#' Make HTTP request with retry logic
#' @param method HTTP method ("GET" or "POST")
#' @param url API endpoint URL
#' @param body Request body (optional)
#' @param ... Additional arguments passed to httr functions
#' @return HTTP response object
#' @importFrom httr GET POST add_headers
#' @noRd
make_request <- function(method, url, body = NULL, ...) {
  MAX_RETRIES <- 5
  retry_count <- 0
  
  while (retry_count < MAX_RETRIES) {
    tryCatch({
      response <- if (method == "GET") {
        GET(url, ...)
      } else {
        POST(
          url,
          body = if (!is.null(body))
            jsonlite::toJSON(body)
          else
            NULL,
          encode = "json",
          add_headers('Content-Type' = 'application/json'),
          ...
        )
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
  json_data <- fromJSON(rawToChar(response$content))
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

#' Wait for API rate limit refresh
#' @description NOTE: the rate limit is server-side, changing this value won't speed up the data retrieval process
#' @noRd
wait_api_cooldown <- function() {
  Sys.sleep(10)  # wait 10 seconds for the soft rate limit reset
}