# Basic arg-validation coverage for st_metrics().
# Live-API behavior is covered in test-live-smoke.R (gated on SENSORTOWER_RUN_LIVE).

test_that("st_metrics rejects missing app_id", {
  expect_error(
    st_metrics(auth_token = "dummy"),
    "app_id"
  )
})

test_that("st_metrics rejects invalid os", {
  expect_error(
    st_metrics(
      app_id     = "553834731",
      os         = "bogus",
      countries  = "US",
      auth_token = "dummy"
    ),
    "must be one of"
  )
})

test_that("st_metrics rejects invalid granularity", {
  expect_error(
    st_metrics(
      app_id      = "553834731",
      os          = "ios",
      countries   = "US",
      granularity = "hourly",
      auth_token  = "dummy"
    ),
    "must be one of"
  )
})

test_that("st_metrics rejects inverted date range", {
  expect_error(
    st_metrics(
      app_id     = "553834731",
      os         = "ios",
      countries  = "US",
      date_from  = "2024-06-01",
      date_to    = "2024-01-01",
      auth_token = "dummy"
    ),
    "must be on or before"
  )
})

test_that("st_metrics rejects malformed country codes", {
  expect_error(
    st_metrics(
      app_id     = "553834731",
      os         = "ios",
      countries  = c("United States"),
      auth_token = "dummy"
    ),
    "2-letter"
  )
})

test_that("st_metrics rejects unknown metrics", {
  expect_error(
    st_metrics(
      app_id     = "553834731",
      os         = "ios",
      countries  = "US",
      metrics    = c("revenue", "bogus_metric"),
      auth_token = "dummy"
    ),
    "Unknown metric"
  )
})
