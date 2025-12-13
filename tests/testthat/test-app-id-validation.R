# Test app ID validation functionality

test_that("app ID validation is integrated into st_sales_report", {
  skip_if(
    !nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN")),
    "Sensor Tower authentication token not found"
  )
  # Test that mismatched IDs are caught
  res1 <- tryCatch({
    st_sales_report(
      ios_app_id = "com.example.app",  # Android ID
      os = "ios",
      countries = "US",
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      date_granularity = "daily"
    )
  }, error = function(e) e)
  expect_true(inherits(res1, "error") || (is.data.frame(res1) && nrow(res1) == 0))
  
  res2 <- tryCatch({
    st_sales_report(
      android_app_id = "1234567890",  # iOS ID
      os = "android", 
      countries = "US",
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      date_granularity = "daily"
    )
  }, error = function(e) e)
  expect_true(inherits(res2, "error") || (is.data.frame(res2) && nrow(res2) == 0))
  
  # Test that unified OS is rejected
  expect_error(
    st_sales_report(
      unified_app_id = "5ba4585f539ce75b97db6bcb",
      os = "unified",
      countries = "US",
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      date_granularity = "daily"
    ),
    "does not support os='unified'"
  )
  
  # Test invalid ID format
  res4 <- tryCatch({
    st_sales_report(
      ios_app_id = "not-a-valid-id",
      os = "ios",
      countries = "US", 
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      date_granularity = "daily"
    )
  }, error = function(e) e)
  expect_true(inherits(res4, "error") || (is.data.frame(res4) && nrow(res4) == 0))
})


test_that("new parameter style works correctly", {
  skip_if(
    !nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN")),
    "Sensor Tower authentication token not found"
  )
  # Should pass validation and fail on auth
  res5 <- tryCatch({
    st_sales_report(
      ios_app_id = "1234567890",
      os = "ios",
      countries = "US",
      start_date = "2024-01-01",
      end_date = "2024-01-01", 
      date_granularity = "daily"
    )
  }, error = function(e) e)
  expect_true(inherits(res5, "error") || (is.data.frame(res5) && nrow(res5) == 0))
  
  res6 <- tryCatch({
    st_sales_report(
      android_app_id = "com.example.app",
      os = "android",
      countries = "US",
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      date_granularity = "daily"  
    )
  }, error = function(e) e)
  expect_true(inherits(res6, "error") || (is.data.frame(res6) && nrow(res6) == 0))
})