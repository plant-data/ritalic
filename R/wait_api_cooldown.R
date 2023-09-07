#' Wait for a specified number of seconds
#'
#' This function waits for a specified number of seconds before continuing.
#' This match the rate limit on the server
#'
#' @param seconds The number of seconds to wait
#' @keywords wait time
wait_api_cooldown <- function(seconds = 10) {
  Sys.sleep(seconds)
}
