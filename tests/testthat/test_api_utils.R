library(testthat)

# a local fake API (a server on 127.0.0.1) that answers with a sequence of status codes (the last one is
# repeated) and records the requests; httr2 mocked responses skip req_retry, so
# the retries are tested against a real (local) HTTP server
local_fake_api <- function(statuses, env = parent.frame()) {
  skip_if_not_installed("webfakes")
  local_mocked_bindings(
    api_cooldown_seconds = function() 0,
    api_failure_wait_seconds = function() 0,
    .env = env
  )

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$locals$calls <- 0
  app$locals$requests <- list()
  app$locals$statuses <- statuses
  answer <- function(req, res) {
    locals <- req$app$locals
    locals$calls <- locals$calls + 1
    locals$requests[[locals$calls]] <- list(method = req$method, json = req$json)
    req$app$locals <- locals
    status <- locals$statuses[min(locals$calls, length(locals$statuses))]
    res$set_status(status)$send_json(list(status = status), auto_unbox = TRUE)
  }
  app$get("/test", answer)
  app$post("/test", answer)
  # the server runs in another process: the recorded requests are read with a request
  app$get("/requests", function(req, res) {
    res$send_json(req$app$locals$requests, auto_unbox = TRUE)
  })

  server <- webfakes::local_app_process(app, .local_envir = env)
  requests <- function() {
    resp <- httr2::req_perform(httr2::request(server$url("/requests")))
    httr2::resp_body_json(resp)
  }
  list(
    url = server$url("/test"),
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
