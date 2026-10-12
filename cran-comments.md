## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission notes

* New functions italic_search() and italic_search_filters() to search the taxa
  by distribution, ecology, traits and taxonomy
* New functions italic_mcp_tools() and italic_mcp_server() to use ritalic from
  AI assistants (packages ellmer and mcptools in Suggests)
* italic_taxon_data() no longer drops the last column of each data source
* Requests to the API now retry at most 5 times (on rate limit, unavailable
  server or network errors) instead of retrying indefinitely
