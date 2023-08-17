#' Define a progress bar
#'
#' This function creates a progress bar using the `txtProgressBar` function.
#'
#' @param n_iterations The number of iterations for the progress bar.
#' @return The progress bar object.
#' @import utils
define_progress_bar <- function(n_iterations) {
  pb <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
                              max = n_iterations, # Maximum value of the progress bar
                              style = 3,    # Progress bar style (also available style = 1 and style = 2)
                              width = 50,   # Progress bar width. Defaults to getOption("width")
                              char = "=")
  return(pb)
}
