library(testthat)

# a local fake API (a server on 127.0.0.1) that records the requests; httr2 mocked
# responses skip req_retry, so the retries are tested against a real (local) HTTP server.
# The server runs in another R process, so a single server is shared by all the tests
# (it starts on the first use and stops at the end of the file); these tests are
# skipped on CRAN, where starting the process can fail on busy machines
fake_api_app <- function() {
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$locals$requests <- list()
  # each test has its own id; the status codes of its answers are in the query
  # (the last one is repeated)
  answer <- function(req, res) {
    id <- req$params$id
    requests <- req$app$locals$requests
    requests[[id]] <- c(requests[[id]], list(list(method = req$method, json = req$json)))
    req$app$locals$requests <- requests
    statuses <- as.integer(strsplit(req$query$statuses, ",")[[1]])
    status <- statuses[min(length(requests[[id]]), length(statuses))]
    res$set_status(status)$send_json(list(status = status), auto_unbox = TRUE)
  }
  app$get("/api/:id", answer)
  app$post("/api/:id", answer)
  app$get("/requests/:id", function(req, res) {
    requests <- req$app$locals$requests[[req$params$id]]
    res$send_json(if (is.null(requests)) list() else requests, auto_unbox = TRUE)
  })
  app
}

if (requireNamespace("webfakes", quietly = TRUE)) {
  fake_api_server <- webfakes::local_app_process(fake_api_app())
}
fake_api_ids <- 0

# a fake API that answers with a sequence of status codes
local_fake_api <- function(statuses, env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("webfakes")
  local_mocked_bindings(
    api_cooldown_seconds = function() 0,
    api_failure_wait_seconds = function() 0,
    .env = env
  )

  fake_api_ids <<- fake_api_ids + 1
  id <- as.character(fake_api_ids)
  requests <- function() {
    resp <- httr2::req_perform(httr2::request(fake_api_server$url(paste0("/requests/", id))))
    httr2::resp_body_json(resp)
  }
  list(
    url = fake_api_server$url(paste0("/api/", id), query = list(statuses = paste(statuses, collapse = ","))),
    calls = function() length(requests()),
    requests = requests
  )
}

describe("make_request function", {
  test_that("returns the response when the status is 200", {
    api <- local_fake_api(200)
    response <- make_request("GET", api$url)
    expect_equal(httr2::resp_status(response), 200)
    expect_equal(api$calls(), 1)
  })

  test_that("retries after a 429 and returns the next successful response", {
    api <- local_fake_api(c(429, 429, 200))
    response <- make_request("GET", api$url)
    expect_equal(httr2::resp_status(response), 200)
    expect_equal(api$calls(), 3)
  })

  test_that("retries after a 503", {
    api <- local_fake_api(c(503, 200))
    response <- make_request("GET", api$url)
    expect_equal(httr2::resp_status(response), 200)
    expect_equal(api$calls(), 2)
  })

  test_that("stops after 5 attempts when the rate limit persists", {
    api <- local_fake_api(429)
    expect_error(
      make_request("GET", api$url),
      "Request failed with status code: 429"
    )
    expect_equal(api$calls(), 5)
  })

  test_that("does not retry a server error", {
    api <- local_fake_api(500)
    expect_error(
      make_request("GET", api$url),
      "Server error - please try again later"
    )
    expect_equal(api$calls(), 1)
  })

  test_that("does not retry a client error", {
    api <- local_fake_api(404)
    expect_error(
      make_request("GET", api$url),
      "Request failed with status code: 404"
    )
    expect_equal(api$calls(), 1)
  })

  test_that("stops after 5 attempts when the request keeps failing", {
    local_mocked_bindings(api_failure_wait_seconds = function() 0)
    retries <- 0
    # nothing listens on port 1 of localhost: the connection is refused
    withCallingHandlers(
      expect_error(
        make_request("GET", "http://127.0.0.1:1/test"),
        "Failed after 5 attempts"
      ),
      httr2_retry = function(cnd) retries <<- retries + 1
    )
    expect_equal(retries, 5)
  })

  test_that("sends the body as JSON in POST requests", {
    api <- local_fake_api(200)
    make_request("POST", api$url, body = list(sp = "Species A"))
    request <- api$requests()[[1]]
    expect_equal(request$method, "post")
    expect_equal(request$json, list(sp = "Species A"))
  })
})
