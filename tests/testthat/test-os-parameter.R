# Test OS parameter behavior

test_that("OS parameter is required in all functions", {
  auth_token <- "dummy_token"
  
  # Test st_metrics
  expect_error(
    st_metrics(
      unified_app_id = "5ba4585f539ce75b97db6bcb",
      countries = "US",
      date_granularity = "monthly",
      auth_token = auth_token
    ),
    "'os' parameter is required"
  )
  
  # Test st_batch_metrics
  expect_error(
    st_batch_metrics(
      app_list = c("553834731", "com.supercell.clashofclans"),
      countries = "US",
      auth_token = auth_token
    ),
    "'os' parameter is required"
  )
  
  # Test st_top_charts - OS is already required
  expect_error(
    st_top_charts(
      category = 6000,
      regions = "US",
      auth_token = auth_token
    ),
    "'os' parameter is required"
  )
})

test_that("OS parameter validates allowed values", {
  auth_token <- "dummy_token"
  
  # Test invalid OS value
  expect_error(
    st_metrics(
      os = "invalid",
      unified_app_id = "5ba4585f539ce75b97db6bcb",
      countries = "US",
      date_granularity = "monthly",
      auth_token = auth_token
    ),
    "must be one of: 'ios', 'android', or 'unified'"
  )
  
  expect_error(
    st_batch_metrics(
      os = 123,
      app_list = c("553834731"),
      countries = "US",
      auth_token = auth_token
    ),
    "must be one of: 'ios', 'android', or 'unified'"
  )
})

test_that("Countries parameter is required with no defaults", {
  auth_token <- "dummy_token"
  
  # Test st_metrics
  expect_error(
    st_metrics(
      os = "unified",
      unified_app_id = "5ba4585f539ce75b97db6bcb",
      date_granularity = "monthly",
      auth_token = auth_token
    ),
    "'countries' parameter is required"
  )
  
  # Test st_batch_metrics
  expect_error(
    st_batch_metrics(
      os = "unified",
      app_list = c("553834731"),
      auth_token = auth_token
    ),
    "'countries' parameter is required"
  )
})

test_that("ID resolution works based on OS parameter", {
  skip_if_not(nchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN")) > 0, 
              "Sensor Tower token not available")
  
  # This test would require actual API calls
  # Skipping for now as it requires valid app IDs and API access
})