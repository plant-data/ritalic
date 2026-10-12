#' Search lichen taxa by distribution, ecology, traits and taxonomy
#'
#' @description
#' Returns the taxa of ITALIC that satisfy a set of filters: administrative regions
#' and ecoregions of Italy, commonness-rarity, substratum, photobiont, growth form,
#' reproductive strategy, ecological indicators (pH, solar irradiation, aridity,
#' eutrophication, poleotolerance, altitudinal distribution) and taxonomy.
#' Different filters are combined with AND, multiple values of the same filter with OR.
#'
#' Use [italic_search_filters()] to list the filters, their allowed values and
#' what each value means.
#'
#' @param ... Filters as named arguments. Values are readable words (e.g.
#'   `substrata = "saxicolous"`), case-insensitive, with spaces and hyphens
#'   equivalent. The filters are:
#'   \describe{
#'     \item{List filters}{`region`, `ecoregion`, `substrata`, `photobiont`,
#'       `growth_form`, `reproductive_strategy`, `phytoclimatic_range`,
#'       `special_requirements_for_water`, `genus`, `family`, `order`, `class`,
#'       `phylum`: a character vector, the taxa matching any of the values are returned}
#'     \item{Range filters}{`commonness`, `ph_of_the_substrata`, `solar_irradiation`,
#'       `aridity`, `eutrophication`, `poleotolerance`, `altitudinal_distribution`:
#'       pass `<filter>_min` and/or `<filter>_max` with a single value (for the
#'       ecological indicators also its number, e.g. `ph_of_the_substrata_min = 4`);
#'       the taxa whose range overlaps the requested one are returned}
#'     \item{Logical filters}{`in_italy`, `coastal`, `parasitic`, `pioneer`,
#'       `metal_rich`, `lichenized`: `TRUE` returns only those taxa, `FALSE` excludes them}
#'   }
#'
#' @return A data frame with one row per taxon and columns:
#'   \describe{
#'     \item{taxon_id}{Identifier of the taxon in ITALIC}
#'     \item{scientific_name}{Accepted name, that can be passed to the other functions of the package}
#'     \item{family}{Family of the taxon}
#'   }
#'
#' @examples
#' \dontrun{
#' # Saxicolous lichens of Friuli Venezia Giulia with cyanobacteria
#' italic_search(region = "friuli-venezia-giulia",
#'               substrata = "saxicolous",
#'               photobiont = "cyanobacteria")
#'
#' # Epiphytic species of slightly basic substrata in the alpine or subalpine ecoregions
#' italic_search(substrata = "epiphytic",
#'               ph_of_the_substrata_min = "slightly-basic",
#'               ecoregion = c("alpine", "subalpine"))
#'
#' # Rare species of the family Teloschistaceae
#' italic_search(family = "Teloschistaceae", commonness_max = "rare")
#'
#' # Then get the data of the taxa found
#' found <- italic_search(genus = "Peltigera", region = "sardegna")
#' italic_taxon_data(found$scientific_name)
#' }
#'
#' @references
#' ITALIC - The Information System on Italian Lichens
#' \url{https://italic.units.it}
#'
#' @importFrom jsonlite fromJSON
#' @export
italic_search <- function(...) {
  filters <- list(...)
  if (length(filters) > 0 &&
      (is.null(names(filters)) || any(names(filters) == ""))) {
    stop("All filters must be named, e.g. italic_search(substrata = \"saxicolous\")")
  }
  # drop empty filters, an empty named list is sent as {}
  filters <- Filter(function(x) !is.null(x) && length(x) > 0, filters)
  if (length(filters) == 0) {
    filters <- setNames(list(), character(0))
  }

  data <- request_italic_v2("taxa-search", body = filters)

  if (length(data$taxa) == 0) {
    return(data.frame(
      taxon_id = integer(0),
      scientific_name = character(0),
      family = character(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    taxon_id = vapply(data$taxa, function(t) as.integer(t$taxon_id), integer(1)),
    scientific_name = vapply(data$taxa, function(t) null_to_na(t$scientific_name), character(1)),
    family = vapply(data$taxa, function(t) null_to_na(t$family), character(1)),
    stringsAsFactors = FALSE
  )
}

#' List the filters of the taxa search
#'
#' @description
#' Lists the filters accepted by [italic_search()], with their allowed values and
#' the meaning of each value as explained on the ITALIC website.
#'
#' @param filter Optional. Name of a filter (e.g. `"substrata"`) or of one of its
#'   parameters (e.g. `"ph_of_the_substrata_min"`): only this filter is returned.
#'
#' @return A data frame with one row per allowed value of each filter, and columns:
#'   \describe{
#'     \item{filter}{Name of the filter}
#'     \item{type}{`list` (one or more values), `range` (`_min` / `_max` parameters)
#'       or `boolean` (`TRUE` / `FALSE`)}
#'     \item{parameters}{Arguments of [italic_search()] for this filter}
#'     \item{value}{Allowed value (`NA` for boolean filters and for `genus`, whose
#'       values are too many to be listed)}
#'     \item{number}{Numeric level of the value, accepted in place of the value
#'       by the ranges of the ecological indicators}
#'     \item{label}{Meaning of the value}
#'     \item{description}{Description of the filter}
#'   }
#'
#' @examples
#' \dontrun{
#' # All the filters
#' italic_search_filters()
#'
#' # Values of the pH of the substrata
#' italic_search_filters("ph_of_the_substrata")
#' }
#'
#' @export
italic_search_filters <- function(filter = NULL) {
  data <- request_italic_v2("taxa-search-options")

  rows <- lapply(data$filters, function(f) {
    values <- f$values
    if (length(values) == 0) {
      values <- list(list())
    }
    data.frame(
      filter = f$filter,
      type = f$type,
      parameters = paste(unlist(f$parameters), collapse = ", "),
      value = vapply(values, function(v) null_to_na(v$value), character(1)),
      number = vapply(values, function(v) if (is.null(v$number)) NA_integer_ else as.integer(v$number), integer(1)),
      label = vapply(values, function(v) null_to_na(v$label), character(1)),
      description = f$description,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)

  if (!is.null(filter)) {
    wanted <- vapply(data$filters, function(f) {
      identical(f$filter, filter) || filter %in% unlist(f$parameters)
    }, logical(1))
    if (!any(wanted)) {
      stop("Unknown filter '", filter, "'. Filters: ",
           paste(unique(result$filter), collapse = ", "))
    }
    result <- result[result$filter %in% data$filters[[which(wanted)[1]]]$filter, ]
  }

  row.names(result) <- NULL
  return(result)
}

#' Call an endpoint of the ITALIC API v2
#' @description GET, or POST with a JSON body when `body` is given. The API returns
#'   readable error messages (e.g. the allowed values of a filter), they are raised as errors.
#' @param endpoint Endpoint after api/v2/
#' @param body Optional list sent as JSON
#' @return The parsed JSON response, not simplified
#' @importFrom jsonlite fromJSON
#' @noRd
request_italic_v2 <- function(endpoint, body = NULL) {
  req <- httr2::request(paste0("https://italic.units.it/api/v2/", endpoint))
  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  }
  # retry when the rate limit is reached, keep the error responses to read their message
  req <- httr2::req_retry(req, max_tries = 4, after = function(resp) 10)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  response <- httr2::req_perform(req)
  data <- fromJSON(httr2::resp_body_string(response), simplifyVector = FALSE)

  status_code <- httr2::resp_status(response)
  if (status_code != 200) {
    if (!is.null(data$error)) {
      stop(data$error, call. = FALSE)
    }
    handle_api_error(status_code)
  }
  return(data)
}

#' Replace NULL with NA
#' @noRd
null_to_na <- function(x) {
  if (is.null(x)) NA_character_ else as.character(x)
}
