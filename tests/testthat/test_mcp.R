library(testthat)

describe("italic_search function", {
  test_that("requires named filters", {
    expect_error(italic_search("saxicolous"), "must be named")
  })
})

describe("mcp_run function", {
  test_that("returns the result as JSON", {
    result <- mcp_run(list(count = 2, names = c("a", "b")))
    expect_equal(jsonlite::fromJSON(result), list(count = 2L, names = c("a", "b")))
  })

  test_that("returns errors as JSON", {
    result <- mcp_run(stop("something went wrong"))
    expect_equal(jsonlite::fromJSON(result), list(error = "something went wrong"))
  })

  test_that("discards printed output", {
    result <- mcp_run({
      cat("printed\n")
      list(ok = TRUE)
    })
    expect_equal(result, "{\"ok\":true}")
  })
})

describe("mcp_names function", {
  test_that("removes duplicates and empty names", {
    expect_equal(mcp_names(c(" A b ", "A b", "", NA, "C d"), 10), c("A b", "C d"))
  })

  test_that("fails when there are no names or too many", {
    expect_error(mcp_names(c("", NA), 10), "at least one name")
    expect_error(mcp_names(c("a", "b", "c"), 2), "maximum is 2")
  })
})

describe("mcp_truncated function", {
  test_that("keeps the first max_results items and reports the truncation", {
    result <- mcp_truncated(list(count = 3), "names", c("a", "b", "c"), 2)
    expect_equal(as.vector(result$names), c("a", "b"))
    expect_match(result$truncated, "first 2 of 3")
  })

  test_that("does not report a truncation when all items fit", {
    result <- mcp_truncated(list(count = 2), "names", c("a", "b"), 5)
    expect_null(result$truncated)
  })
})

describe("italic_mcp_tools function", {
  test_that("returns a tool for each function", {
    skip_if_not_installed("ellmer")
    tools <- italic_mcp_tools()
    expect_length(tools, 6)
  })

  test_that("the search tool has an argument for each filter", {
    skip_if_not_installed("ellmer")
    search <- mcp_tool_search()
    expect_true(all(c("region", "substrata", "ph_of_the_substrata_min", "family", "max_results")
                    %in% names(formals(search))))
    expect_null(formals(search)$region)
  })
})
