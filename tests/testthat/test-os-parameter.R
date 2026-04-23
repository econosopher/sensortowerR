# Test OS parameter behavior on v1.0.0 unified functions

test_that("st_metrics validates OS parameter values", {
  expect_error(
    st_metrics(
      app_id     = "5ba4585f539ce75b97db6bcb",
      os         = "invalid",
      countries  = "US",
      auth_token = "dummy"
    ),
    "must be one of"
  )
})

test_that("st_metrics accepts the three allowed OS values", {
  # Each `os` value is accepted at the arg layer. Further execution may
  # succeed (return a tibble) or fail (network/auth) — we only care that
  # the "must be one of" validation path isn't triggered.
  for (v in c("ios", "android", "unified")) {
    res <- tryCatch(
      st_metrics(
        app_id     = "5ba4585f539ce75b97db6bcb",
        os         = v,
        countries  = "US",
        date_from  = "2024-01-01",
        date_to    = "2024-01-01",
        auth_token = "dummy"
      ),
      error = function(e) e
    )
    msg <- if (inherits(res, "condition")) conditionMessage(res) else ""
    expect_false(grepl("must be one of", msg))
  }
})

test_that("st_rankings validates entity and os", {
  expect_error(
    st_rankings(
      entity     = "bogus",
      os         = "ios",
      category   = 6000,
      country    = "US",
      auth_token = "dummy"
    )
  )
  expect_error(
    st_rankings(
      entity     = "app",
      os         = "invalid",
      category   = 6000,
      country    = "US",
      auth_token = "dummy"
    ),
    "must be one of"
  )
})

test_that("ID resolution works based on OS parameter", {
  skip_if_not(nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN")),
              "Sensor Tower token not available")
  skip_on_cran()
  # Live-API test. Skipped in CRAN checks.
})
