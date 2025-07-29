# Test end_dates parameter in st_ytd_metrics

test_that("end_dates parameter works correctly", {
  skip_if_not(nchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN")) > 0, 
              "Sensor Tower token not available")
  
  # Test with single end date
  expect_error(
    st_ytd_metrics(
      os = "unified",
      unified_app_id = "5ba4585f539ce75b97db6bcb",
      countries = "US",
      end_dates = "2024-06-30",
      auth_token = "dummy"
    ),
    NA  # Should not error on validation
  )
  
  # Test with multiple end dates
  expect_error(
    st_ytd_metrics(
      os = "unified",
      unified_app_id = "5ba4585f539ce75b97db6bcb",
      countries = "US",
      end_dates = c("2024-03-31", "2024-09-30", "2025-03-31"),
      auth_token = "dummy"
    ),
    NA  # Should not error on validation
  )
  
  # Test with Date objects
  expect_error(
    st_ytd_metrics(
      os = "unified",
      unified_app_id = "5ba4585f539ce75b97db6bcb",
      countries = "US",
      end_dates = as.Date(c("2024-06-30", "2025-06-30")),
      auth_token = "dummy"
    ),
    NA  # Should not error on validation
  )
})

test_that("end_dates overrides years with warning", {
  expect_warning(
    st_ytd_metrics(
      os = "unified",
      unified_app_id = "5ba4585f539ce75b97db6bcb",
      countries = "US",
      end_dates = "2024-06-30",
      years = 2023,  # This should be ignored
      auth_token = "dummy",
      verbose = FALSE
    ),
    "Both 'end_dates' and 'years' specified"
  )
})

test_that("years parameter shows deprecation message", {
  expect_message(
    st_ytd_metrics(
      os = "unified",
      unified_app_id = "5ba4585f539ce75b97db6bcb",
      countries = "US",
      years = 2024,
      auth_token = "dummy",
      verbose = TRUE
    ),
    "deprecated"
  )
})