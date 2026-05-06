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

test_that("st_metrics normalization removes identical duplicate metric rows", {
  raw <- tibble::tibble(
    original_id = c("com.example.game", "com.example.game", "com.example.game"),
    app_id = c("com.example.game", "com.example.game", "com.example.game"),
    country = c("US", "US", "US"),
    date = as.Date(c("2026-04-27", "2026-04-27", "2026-04-27")),
    metric = c("revenue", "revenue", "downloads"),
    value = c(100, 100, 10)
  )

  normalized <- sensortowerR:::.st_metrics_normalize_output(
    data = raw,
    requested_ids = "com.example.game",
    requested_countries = "US",
    os = "android",
    metrics = c("revenue", "downloads"),
    revenue_unit = "dollars",
    shape = "long"
  )

  expect_equal(nrow(normalized), 2)
  expect_equal(
    normalized$value[normalized$metric == "revenue"],
    100
  )
})

test_that("st_metrics normalization rejects conflicting duplicate metric rows", {
  raw <- tibble::tibble(
    original_id = c("com.example.game", "com.example.game"),
    app_id = c("com.example.game", "com.example.game"),
    country = c("US", "US"),
    date = as.Date(c("2026-04-27", "2026-04-27")),
    metric = c("revenue", "revenue"),
    value = c(100, 101)
  )

  expect_error(
    sensortowerR:::.st_metrics_normalize_output(
      data = raw,
      requested_ids = "com.example.game",
      requested_countries = "US",
      os = "android",
      metrics = c("revenue", "downloads"),
      revenue_unit = "dollars",
      shape = "long"
    ),
    "conflicting duplicate metric values"
  )
})

test_that("st_metrics normalization filters rows outside requested countries", {
  raw <- tibble::tibble(
    original_id = c("123", "123", "123", "123"),
    app_id = c("123", "123", "123", "123"),
    country = c("CN", "CN", "US", "US"),
    date = as.Date(rep("2026-04-27", 4)),
    metric = c("revenue", "downloads", "revenue", "downloads"),
    value = c(100, 10, 999, 99)
  )

  normalized <- sensortowerR:::.st_metrics_normalize_output(
    data = raw,
    requested_ids = "123",
    requested_countries = "CN",
    os = "ios",
    metrics = c("revenue", "downloads"),
    revenue_unit = "dollars",
    shape = "wide"
  )

  expect_equal(unique(normalized$country), "CN")
  expect_equal(normalized$revenue, 100)
  expect_equal(normalized$downloads, 10)
})

test_that("st_metrics wide normalization preserves metric columns after country filter removes all rows", {
  raw <- tibble::tibble(
    original_id = "123",
    app_id = "123",
    country = "US",
    date = as.Date("2026-04-27"),
    metric = "revenue",
    value = 999
  )

  normalized <- sensortowerR:::.st_metrics_normalize_output(
    data = raw,
    requested_ids = "123",
    requested_countries = "CN",
    os = "ios",
    metrics = c("revenue", "downloads"),
    revenue_unit = "dollars",
    shape = "wide"
  )

  expect_equal(nrow(normalized), 0)
  expect_true(all(c("revenue", "downloads") %in% names(normalized)))
})
