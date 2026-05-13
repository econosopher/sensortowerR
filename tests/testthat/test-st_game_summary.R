game_summary_response <- function(payload) {
  httr2::response(
    status_code = 200,
    body = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))
  )
}

test_that("iOS game summary converts cents to dollars before combining revenue", {
  payload <- list(
    list(
      ca = 7001,
      cc = "US",
      d = "2024-01-01T00:00:00Z",
      iu = 10,
      au = 5,
      ir = 2147483647,
      ar = 2147483647
    )
  )

  result <- sensortowerR:::process_game_summary_response(
    game_summary_response(payload),
    os = "ios"
  )

  expect_equal(result$`iOS Downloads`, 15)
  expect_type(result$`iOS Revenue`, "double")
  expect_equal(result$`iOS Revenue`, 42949672.94)
  expect_false(any(is.na(result$`iOS Revenue`)))
})

test_that("Android game summary maps downloads and dollar revenue", {
  payload <- list(
    list(
      ca = "game_action",
      cc = "US",
      d = "2024-01-01T00:00:00Z",
      u = 123,
      r = 456789
    )
  )

  result <- sensortowerR:::process_game_summary_response(
    game_summary_response(payload),
    os = "android"
  )

  expect_equal(result$Category, "game_action")
  expect_equal(result$`Country Code`, "US")
  expect_equal(result$Date, as.Date("2024-01-01"))
  expect_equal(result$`Android Downloads`, 123)
  expect_equal(result$`Android Revenue`, 4567.89)
})

test_that("unified game summary aggregates iOS and Android endpoint rows", {
  requested_urls <- character()

  testthat::local_mocked_bindings(
    perform_request = function(req) {
      requested_urls <<- c(requested_urls, req$url)

      if (grepl("/v1/ios/games_breakdown", req$url, fixed = TRUE)) {
        return(game_summary_response(list(
          list(ca = 7001, cc = "US", d = "2024-01-01T00:00:00Z", iu = 10, au = 5, ir = 1000, ar = 500)
        )))
      }

      if (grepl("/v1/android/games_breakdown", req$url, fixed = TRUE)) {
        return(game_summary_response(list(
          list(ca = "game_action", cc = "US", d = "2024-01-01T00:00:00Z", u = 20, r = 2500)
        )))
      }

      stop("Unexpected request URL: ", req$url)
    },
    .package = "sensortowerR"
  )

  result <- st_game_summary(
    categories = c("7001", "game_action"),
    countries = "US",
    os = "unified",
    date_granularity = "daily",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    auth_token = "token"
  )

  expect_equal(length(requested_urls), 2)
  expect_true(any(grepl("/v1/ios/games_breakdown", requested_urls, fixed = TRUE)))
  expect_true(any(grepl("/v1/android/games_breakdown", requested_urls, fixed = TRUE)))
  expect_equal(nrow(result), 1)
  expect_equal(result$`Total Downloads`, 35)
  expect_equal(result$`Total Revenue`, 40)
})

test_that("game summary uses aggregate breakdown endpoint, not title-batch paths", {
  requested_urls <- character()
  title_batch_error <- function(...) {
    stop("title-batch path should not be used for market totals")
  }

  testthat::local_mocked_bindings(
    perform_request = function(req) {
      requested_urls <<- c(requested_urls, req$url)
      game_summary_response(list(
        list(ca = 7001, cc = "US", d = "2024-01-01T00:00:00Z", iu = 1, au = 1, ir = 100, ar = 100)
      ))
    },
    st_batch_metrics_impl = title_batch_error,
    st_top_charts_impl = title_batch_error,
    .package = "sensortowerR"
  )

  result <- st_game_summary(
    categories = 7001,
    countries = "US",
    os = "ios",
    date_granularity = "daily",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    auth_token = "token"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$`iOS Revenue`, 2)
  expect_equal(length(requested_urls), 1)
  expect_true(grepl("/v1/ios/games_breakdown", requested_urls, fixed = TRUE))
  expect_false(grepl("ranking|top_and_trending|timeseries|sales_report", requested_urls))
})
