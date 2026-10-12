#' Tools of the ITALIC MCP server
#'
#' @description
#' Returns the functions of ritalic as tools for AI assistants, created with
#' `ellmer::tool()`. They can be served to Claude, Positron or other MCP clients
#' with [italic_mcp_server()], or registered in an `ellmer` chat with
#' `chat$register_tools(italic_mcp_tools())`.
#'
#' The tools are:
#' \describe{
#'   \item{italic_match}{Aligns scientific names to ITALIC, dropping the matches below a minimum score}
#'   \item{italic_taxon_data}{Taxonomy, ecology and traits, distribution in regions and ecoregions,
#'     description and functional traits of the taxa, as a table}
#'   \item{italic_search}{Searches the taxa by distribution, ecology, traits and taxonomy}
#'   \item{italic_search_filters}{Lists the filters of the search and the meaning of their values}
#'   \item{italic_identification_key}{Creates an interactive identification key with the KeyMaker}
#'   \item{italic_checklist}{Lists the accepted names of the checklist}
#' }
#'
#' Each tool returns JSON text; errors are returned as `{"error": "..."}`.
#'
#' @return A list of `ellmer` tool definitions.
#'
#' @examples
#' \dontrun{
#' # Use the tools in an ellmer chat
#' chat <- ellmer::chat_anthropic()
#' chat$register_tools(italic_mcp_tools())
#' chat$chat("Which saxicolous lichens with cyanobacteria grow in Sardegna?")
#' }
#'
#' @seealso [italic_mcp_server()]
#' @export
italic_mcp_tools <- function() {
  check_mcp_packages("ellmer")

  read_only <- function(title) {
    ellmer::tool_annotations(
      title = title,
      read_only_hint = TRUE,
      open_world_hint = FALSE
    )
  }

  list(
    ellmer::tool(
      mcp_tool_match,
      name = "italic_match",
      description = paste(
        "Align scientific names to the nomenclature of ITALIC, the information system on the lichens of Italy.",
        "ALWAYS use it first on the names of a user's dataset: the other tools need the exact accepted names returned here.",
        "Each distinct name gets an alignment: 'accepted' or 'synonym' (kept: use its accepted_name),",
        "'low_score' (dropped: the best_candidate is shown, tell the user so that they can fix the name, never use it),",
        "'not_found' (dropped). The summary reports the input names merged into the same accepted taxon: point them out.",
        "For a dataset, pass its distinct names and join the results back to the rows by input_name.",
        sprintf("Maximum %d names per call.", MCP_MAX_NAMES)
      ),
      arguments = list(
        sp_names = ellmer::type_array(
          items = ellmer::type_string(),
          description = "Scientific names, with or without authorship"
        ),
        min_score = ellmer::type_integer(
          description = sprintf("Matches with a lower name score (0-100) are dropped. Default %d", MCP_DEFAULT_MIN_SCORE),
          required = FALSE
        )
      ),
      annotations = read_only("Align names to ITALIC")
    ),

    ellmer::tool(
      mcp_tool_taxon_data,
      name = "italic_taxon_data",
      description = paste(
        "Data of ITALIC taxa as a table (one row per taxon), to be joined to the user's dataset on the accepted name.",
        "Pass the exact accepted names (with authorship) returned by italic_match or italic_search.",
        "Data:",
        "taxonomy (phylum, class, order, family, genus);",
        "ecology_traits (substrata, photobiont, growth form, reproduction, ranges of the ecological indicators:",
        "pH of the substrata, solar irradiation, aridity, eutrophication, poleotolerance, altitudinal distribution);",
        "ecoregions_distribution (commonness-rarity in the 9 ecoregions of Italy, columns ecoregion_*);",
        "regions_distribution (presence in the administrative regions of Italy, columns region_*);",
        "description (morphological description and notes, long texts: ask for it only for a few taxa);",
        "functional_traits (presence/absence of morpho-functional traits, columns trait_*).",
        sprintf("Maximum %d taxa per call, each one takes a request per data: ask only the data you need.", MCP_MAX_TAXA)
      ),
      arguments = list(
        sp_names = ellmer::type_array(
          items = ellmer::type_string(),
          description = "Accepted names of ITALIC, with authorship"
        ),
        data = ellmer::type_array(
          items = ellmer::type_enum(values = names(mcp_taxon_data_sources())),
          description = paste("Data to retrieve. Default:", paste(MCP_DEFAULT_DATA, collapse = ", ")),
          required = FALSE
        )
      ),
      annotations = read_only("Get taxon data")
    ),

    ellmer::tool(
      mcp_tool_search(),
      name = "italic_search",
      description = paste(
        "Search the taxa of ITALIC that satisfy all the given filters (different filters: AND; several values of one filter: OR).",
        "Use readable values, never database codes (e.g. substrata ['saxicolous'], region ['veneto'], ph_of_the_substrata_min 'slightly-basic').",
        "Call italic_search_filters to know the allowed values and their meaning, and explain the relevant options to the user;",
        "ask when the request is ambiguous (e.g. 'in the Alps' -> ecoregion alpine/subalpine or the alpine regions).",
        "Returns the count and the taxa (taxon_id, scientific_name, family); pass the scientific_name to italic_taxon_data for details."
      ),
      arguments = mcp_search_arguments(),
      annotations = read_only("Search taxa")
    ),

    ellmer::tool(
      mcp_tool_search_filters,
      name = "italic_search_filters",
      description = paste(
        "List the filters of italic_search, their allowed values and what each value means (as explained on the ITALIC website).",
        "Use the labels to explain the options to the user. Filter types: 'list' (one or more values, any of them),",
        "'range' (_min/_max parameters, taxa whose range overlaps the requested one; the value or its number),",
        "'boolean' (true: only those taxa, false: exclude them)."
      ),
      arguments = list(
        filter = ellmer::type_string(
          description = "Show only this filter (e.g. 'substrata', 'family'); all when omitted",
          required = FALSE
        )
      ),
      annotations = read_only("List the search filters")
    ),

    ellmer::tool(
      mcp_tool_identification_key,
      name = "italic_identification_key",
      description = paste(
        "Create an interactive identification key to a set of taxa with the ITALIC KeyMaker, and return its link.",
        "ALWAYS use this tool when the user asks for an identification key (also called dichotomous or interactive key)",
        "to a list of species: never write a key yourself from the descriptions.",
        "Pass the exact accepted names returned by italic_match or italic_search (at least two).",
        "Only the taxa with identification data are included in the key: tell the user to check it."
      ),
      arguments = list(
        sp_names = ellmer::type_array(
          items = ellmer::type_string(),
          description = "Accepted names of ITALIC, with authorship"
        )
      ),
      annotations = ellmer::tool_annotations(
        title = "Create an identification key",
        read_only_hint = FALSE,
        idempotent_hint = TRUE,
        destructive_hint = FALSE,
        open_world_hint = FALSE
      )
    ),

    ellmer::tool(
      mcp_tool_checklist,
      name = "italic_checklist",
      description = paste(
        "Accepted names of the checklist of the lichens of Italy, optionally of a genus, family, order, class or phylum,",
        "and optionally including the taxa known only from the bordering countries.",
        "To filter by distribution, ecology or traits use italic_search."
      ),
      arguments = list(
        include_bordering_countries = ellmer::type_boolean(
          description = "Include the taxa known only from the bordering countries. Default false",
          required = FALSE
        ),
        genus = ellmer::type_string(description = "Genus name, without authorship", required = FALSE),
        family = ellmer::type_string(description = "Family name, without authorship", required = FALSE),
        order = ellmer::type_string(description = "Order name, without authorship", required = FALSE),
        class = ellmer::type_string(description = "Class name, without authorship", required = FALSE),
        phylum = ellmer::type_string(description = "Phylum name, without authorship", required = FALSE),
        max_results = ellmer::type_integer(
          description = sprintf("Maximum names returned (the count is always the full one). Default %d", MCP_DEFAULT_MAX_RESULTS),
          required = FALSE
        )
      ),
      annotations = read_only("Get the checklist")
    )
  )
}

#' Run the ITALIC MCP server
#'
#' @description
#' Runs a local [MCP](https://modelcontextprotocol.io) server that lets AI assistants
#' (Claude, Cursor, ...) work with ITALIC through the functions of ritalic: aligning
#' names, retrieving the data of the taxa, searching taxa and creating identification
#' keys. The tools are listed in [italic_mcp_tools()].
#'
#' The server communicates over stdin/stdout and is meant to be started by the MCP
#' client, not in an interactive session. Requires the packages `mcptools` and `ellmer`.
#'
#' @section Connecting an assistant:
#' Claude Code:
#' \preformatted{
#' claude mcp add ritalic -- Rscript -e "ritalic::italic_mcp_server()"
#' }
#' Claude Desktop, in `claude_desktop_config.json`:
#' \preformatted{
#' {
#'   "mcpServers": {
#'     "ritalic": {
#'       "command": "Rscript",
#'       "args": ["-e", "ritalic::italic_mcp_server()"]
#'     }
#'   }
#' }
#' }
#' Use the full path of `Rscript` if it is not found (e.g. the output of
#' `file.path(R.home("bin"), "Rscript")`).
#'
#' @param ... Further arguments passed to `mcptools::mcp_server()`.
#'
#' @return Does not return: it serves the tools until the client closes the connection.
#'
#' @seealso [italic_mcp_tools()]
#' @export
italic_mcp_server <- function(...) {
  check_mcp_packages(c("ellmer", "mcptools"))
  mcptools::mcp_server(tools = italic_mcp_tools(), ...)
}


# ---------- limits ----------

MCP_DEFAULT_MIN_SCORE <- 90
MCP_MAX_NAMES <- 500
MCP_MAX_TAXA <- 100
MCP_DEFAULT_MAX_RESULTS <- 500
MCP_DEFAULT_DATA <- c("taxonomy", "ecology_traits", "ecoregions_distribution", "regions_distribution")


# ---------- tool functions ----------

#' @noRd
mcp_tool_match <- function(sp_names, min_score = MCP_DEFAULT_MIN_SCORE) {
  mcp_run({
    sp_names <- mcp_names(sp_names, MCP_MAX_NAMES)
    res <- italic_match(sp_names)

    score <- suppressWarnings(as.numeric(res$name_score))
    found <- !is.na(res$matched_name)
    low <- found & (is.na(score) | score < min_score)
    kept <- found & !low & !is.na(res$accepted_name)
    alignment <- ifelse(!found, "not_found",
                        ifelse(low, "low_score",
                               ifelse(is.na(res$status), "no_accepted_name", res$status)))

    aligned <- data.frame(
      input_name = res$input_name,
      alignment = alignment,
      kept = kept,
      name_score = score,
      matched_name = ifelse(kept, res$matched_name, NA),
      accepted_name = ifelse(kept, res$accepted_name, NA),
      best_candidate = ifelse(low, res$matched_name, NA),
      stringsAsFactors = FALSE
    )

    by_taxon <- split(aligned$input_name[kept], aligned$accepted_name[kept])
    merged <- by_taxon[lengths(by_taxon) > 1]
    merged <- lapply(names(merged), function(accepted) {
      list(accepted_name = accepted, input_names = merged[[accepted]])
    })

    list(
      summary = list(
        min_score = min_score,
        distinct_input_names = nrow(aligned),
        kept = sum(kept),
        kept_as_accepted_name = sum(alignment == "accepted"),
        kept_as_synonym = sum(alignment == "synonym"),
        dropped_low_score = sum(alignment == "low_score"),
        dropped_not_found = sum(alignment == "not_found"),
        distinct_accepted_names = length(by_taxon),
        input_names_merged_into_same_taxon = merged
      ),
      names = aligned
    )
  })
}

#' @noRd
mcp_tool_taxon_data <- function(sp_names, data = MCP_DEFAULT_DATA) {
  mcp_run({
    sp_names <- mcp_names(sp_names, MCP_MAX_TAXA)
    sources <- mcp_taxon_data_sources()
    unknown <- setdiff(data, names(sources))
    if (length(unknown) > 0) {
      stop("Invalid data: ", paste(unknown, collapse = ", "),
           ". Allowed values: ", paste(names(sources), collapse = ", "))
    }

    taxa_table <- data.frame(scientific_name = sp_names, stringsAsFactors = FALSE)
    for (kind in unique(data)) {
      src <- sources[[kind]]
      values <- src$fun(sp_names)
      # the first column is the scientific name, already in the table
      values <- values[, -1, drop = FALSE]
      values <- values[, !colnames(values) %in% c("warning", "warnings"), drop = FALSE]
      colnames(values) <- paste0(src$prefix, colnames(values))
      taxa_table <- cbind(taxa_table, values)
    }

    # the API returns empty data for the names that are not in ITALIC
    data_columns <- taxa_table[, -1, drop = FALSE]
    not_found <- taxa_table$scientific_name[rowSums(!is.na(data_columns)) == 0]

    result <- list(
      table = list(columns = colnames(taxa_table), rows = taxa_table),
      names_not_found = I(not_found)
    )
    if (length(not_found) > 0) {
      result$note <- "Names without data are not accepted names of ITALIC: align them with italic_match first"
    }
    result
  }, dataframe = "values")
}

#' @noRd
mcp_tool_search_filters <- function(filter = NULL) {
  mcp_run({
    if (!is.null(filter) && !nzchar(filter)) {
      filter <- NULL
    }
    options <- italic_search_filters(filter)
    groups <- split(options, factor(options$filter, levels = unique(options$filter)))
    filters <- lapply(groups, function(x) {
      values <- x[!is.na(x$value), c("value", "number", "label"), drop = FALSE]
      values <- values[, colSums(!is.na(values)) > 0, drop = FALSE]
      row.names(values) <- NULL
      list(
        filter = x$filter[1],
        type = x$type[1],
        parameters = x$parameters[1],
        description = x$description[1],
        values = values
      )
    })
    list(
      usage = paste(
        "Pass the filters to italic_search: different filters are combined with AND, several values of one filter with OR.",
        "Values are case-insensitive; spaces and hyphens are equivalent."
      ),
      filters = unname(filters)
    )
  })
}

#' @noRd
mcp_tool_identification_key <- function(sp_names) {
  mcp_run({
    sp_names <- mcp_names(sp_names, MCP_MAX_NAMES)
    if (length(sp_names) < 2) {
      stop("At least two taxa are needed to create an identification key")
    }
    link <- italic_identification_key(sp_names)
    if (!is.character(link) || length(link) != 1) {
      stop("The KeyMaker could not create a key for these taxa: check that they are accepted names of ITALIC")
    }
    list(
      link = link,
      taxa = I(sp_names),
      note = paste(
        "Interactive key on the ITALIC website: the user answers the questions on the characters and the",
        "candidate species narrow down. Taxa without identification data are not included in the key."
      )
    )
  })
}

#' @noRd
mcp_tool_checklist <- function(include_bordering_countries = FALSE,
                               genus = NULL,
                               family = NULL,
                               order = NULL,
                               class = NULL,
                               phylum = NULL,
                               max_results = MCP_DEFAULT_MAX_RESULTS) {
  mcp_run({
    checklist <- italic_checklist(
      include_bordering_countries = isTRUE(include_bordering_countries),
      genus = genus,
      family = family,
      order = order,
      class = class,
      phylum = phylum
    )
    mcp_truncated(list(count = length(checklist)), "names", checklist, max_results)
  })
}

#' Tool function of the search, with an argument for each filter
#' @noRd
mcp_tool_search <- function() {
  filter_names <- setdiff(names(mcp_search_arguments()), "max_results")

  fun <- function(max_results = MCP_DEFAULT_MAX_RESULTS) {
    mcp_run({
      filters <- mget(filter_names, envir = environment())
      filters <- Filter(function(x) !is.null(x) && length(x) > 0, filters)
      taxa <- do.call(italic_search, filters)
      result <- list(filters = if (length(filters) > 0) filters else setNames(list(), character(0)),
                     count = nrow(taxa))
      mcp_truncated(result, "taxa", taxa, max_results)
    })
  }
  # an argument for each filter, before max_results
  formals(fun) <- c(
    setNames(rep(list(NULL), length(filter_names)), filter_names),
    formals(fun)
  )
  fun
}

#' Arguments of the search tool: the filters of italic_search()
#' @noRd
mcp_search_arguments <- function() {
  values_hint <- " (values from italic_search_filters)"
  list_filter <- function(description) {
    ellmer::type_array(
      items = ellmer::type_string(),
      description = paste0(description, values_hint, ", any of them"),
      required = FALSE
    )
  }
  range_bound <- function(description, bound) {
    ellmer::type_string(
      description = paste0(description, ", ", bound, values_hint, "; the value or its number"),
      required = FALSE
    )
  }
  flag <- function(description) {
    ellmer::type_boolean(description = description, required = FALSE)
  }
  taxon <- function(rank) {
    ellmer::type_array(
      items = ellmer::type_string(),
      description = paste(rank, "names, without authorship, any of them"),
      required = FALSE
    )
  }

  ranges <- c(
    commonness = "Commonness-rarity in the selected ecoregions (in any ecoregion if none is selected)",
    ph_of_the_substrata = "pH of the substratum",
    solar_irradiation = "Solar irradiation",
    aridity = "Aridity",
    eutrophication = "Eutrophication",
    poleotolerance = "Poleotolerance (tolerance to human disturbance)",
    altitudinal_distribution = "Altitudinal distribution (vegetation belts)"
  )
  range_arguments <- list()
  for (name in names(ranges)) {
    range_arguments[[paste0(name, "_min")]] <- range_bound(ranges[[name]], "minimum")
    range_arguments[[paste0(name, "_max")]] <- range_bound(ranges[[name]], "maximum")
  }

  c(
    list(
      region = list_filter("Administrative regions of Italy where the taxon has been reported, e.g. 'veneto', 'friuli-venezia-giulia'"),
      ecoregion = list_filter("Ecoregions of Italy where the taxon is present, e.g. 'alpine', 'dry-mediterranean'"),
      in_italy = flag("true: only taxa present in Italy; false: only taxa to be looked for in Italy (known from neighbouring countries)"),
      substrata = list_filter("Substratum, e.g. 'saxicolous', 'epiphytic'"),
      photobiont = list_filter("Photosynthetic partner of the lichen, e.g. 'green-algae', 'cyanobacteria'"),
      growth_form = list_filter("Growth form of the thallus, a general form includes its subtypes, e.g. 'foliose', 'crustose-endolithic'"),
      reproductive_strategy = list_filter("Main reproductive strategy, e.g. 'sexual', 'asexual-soredia'"),
      phytoclimatic_range = list_filter("Phytoclimatic range, e.g. 'oceanic'"),
      special_requirements_for_water = list_filter("Special requirements for water, e.g. 'rain-sheltered'"),
      coastal = flag("true: only coastal species; false: exclude them"),
      parasitic = flag("true: only species parasitic on other lichens; false: exclude them"),
      pioneer = flag("true: only pioneer species; false: exclude them"),
      metal_rich = flag("true: only species of metal-rich substrata; false: exclude them"),
      lichenized = flag("true: only lichenized species; false: only non-lichenized species (lichenicolous and other fungi)")
    ),
    range_arguments,
    list(
      genus = taxon("Genus"),
      family = taxon("Family"),
      order = taxon("Order"),
      class = taxon("Class"),
      phylum = taxon("Phylum"),
      max_results = ellmer::type_integer(
        description = sprintf("Maximum taxa returned (the count is always the full one). Default %d", MCP_DEFAULT_MAX_RESULTS),
        required = FALSE
      )
    )
  )
}


# ---------- helpers ----------

#' Functions retrieving each kind of taxon data, and the prefix of their columns
#' @noRd
mcp_taxon_data_sources <- function() {
  list(
    taxonomy = list(fun = italic_taxonomy, prefix = ""),
    ecology_traits = list(fun = italic_ecology_traits, prefix = ""),
    ecoregions_distribution = list(fun = italic_ecoregions_distribution, prefix = "ecoregion_"),
    regions_distribution = list(fun = italic_regions_distribution, prefix = "region_"),
    description = list(fun = italic_description, prefix = ""),
    functional_traits = list(fun = italic_traits_pa, prefix = "trait_")
  )
}

#' Distinct non-empty names, at most max
#' @noRd
mcp_names <- function(sp_names, max) {
  sp_names <- unique(trimws(as.character(unlist(sp_names))))
  sp_names <- sp_names[!is.na(sp_names) & nzchar(sp_names)]
  if (length(sp_names) == 0) {
    stop("sp_names must contain at least one name")
  }
  if (length(sp_names) > max) {
    stop(length(sp_names), " names, the maximum is ", max, ": split them in several calls")
  }
  sp_names
}

#' Adds the first max_results items of a vector or data frame to a result
#' @noRd
mcp_truncated <- function(result, field, items, max_results) {
  max_results <- if (is.null(max_results)) MCP_DEFAULT_MAX_RESULTS else max(1, as.integer(max_results))
  total <- NROW(items)
  result[[field]] <- if (is.data.frame(items)) utils::head(items, max_results) else I(utils::head(items, max_results))
  if (total > max_results) {
    result$truncated <- sprintf(
      "Only the first %d of %d %s are shown: raise max_results or add filters", max_results, total, field
    )
  }
  result
}

#' Evaluates a tool, returning its result or its error as JSON text
#' @description Anything printed is discarded: on the stdio transport stdout is
#'   reserved to the protocol.
#' @noRd
mcp_run <- function(expr, dataframe = "rows") {
  result <- tryCatch({
    utils::capture.output(value <- expr)
    value
  }, error = function(e) {
    list(error = conditionMessage(e))
  })
  as.character(jsonlite::toJSON(
    result,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = dataframe,
    digits = NA
  ))
}

#' Stops when the packages needed by the MCP server are not installed
#' @noRd
check_mcp_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("The MCP server needs the package(s) ", paste(missing, collapse = ", "),
         ": install them with install.packages(c(",
         paste0("\"", missing, "\"", collapse = ", "), "))", call. = FALSE)
  }
}
