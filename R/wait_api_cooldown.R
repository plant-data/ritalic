#' Wait for a specified number of seconds
#'
#' This function waits for a specified number of seconds before continuing.
#' NOTE: the rate limit is server-side, changing this value won't speed up the functions
#'
#' @param seconds The number of seconds to wait
#' @keywords wait time
wait_api_cooldown <- function(seconds = 10) {
  Sys.sleep(seconds)
}
