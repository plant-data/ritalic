#' Create a progress bar
#' @param total_items Total number of items to process
#' @param title Optional title for the progress bar (default: "")
#' @return Progress bar object or NULL if non-interactive
#' @importFrom utils txtProgressBar
#' @noRd
create_progress_bar <- function(total_items, title = "") {
  if (!interactive()) {
    return(NULL)
  }
  
  if (nchar(title) > 0) {
    message(title)
  }
  
  txtProgressBar(
    min = 0,
    max = total_items,
    style = 3,
    width = 50,
    char = "="
  )
}

#' Update progress bar
#' @param pb Progress bar object
#' @param value Current progress value
#' @importFrom utils setTxtProgressBar
#' @noRd
update_progress <- function(pb, value) {
  if (!is.null(pb) && interactive()) {
    setTxtProgressBar(pb, value)
  }
}

#' Close progress bar
#' @param pb Progress bar object
#' @noRd
close_progress_bar <- function(pb) {
  if (!is.null(pb) && interactive()) {
    close(pb)
    message("Completed!")
  }
}