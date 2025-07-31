# Test app ID validation functionality

test_that("app ID validation is integrated into st_sales_report", {
  # Test that mismatched IDs are caught
  expect_error(
    st_sales_report(
      ios_app_id = "com.example.app",  # Android ID
      os = "ios",
      countries = "US",
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      date_granularity = "daily"
    ),
    "Android app ID provided but iOS ID required"
  )
  
  expect_error(
    st_sales_report(
      android_app_id = "1234567890",  # iOS ID
      os = "android", 
      countries = "US",
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      date_granularity = "daily"
    ),
    "iOS app ID provided but Android ID required"
  )
  
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
    "st_sales_report does not support os='unified'"
  )
  
  # Test invalid ID format
  expect_error(
    st_sales_report(
      ios_app_id = "not-a-valid-id",
      os = "ios",
      countries = "US", 
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      date_granularity = "daily"
    ),
    "Invalid app ID format"
  )
})


test_that("new parameter style works correctly", {
  # Should pass validation and fail on auth
  expect_error(
    st_sales_report(
      ios_app_id = "1234567890",
      os = "ios",
      countries = "US",
      start_date = "2024-01-01",
      end_date = "2024-01-01", 
      date_granularity = "daily"
    ),
    "Authentication token"
  )
  
  expect_error(
    st_sales_report(
      android_app_id = "com.example.app",
      os = "android",
      countries = "US",
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      date_granularity = "daily"  
    ),
    "Authentication token"
  )
})