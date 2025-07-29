test_that("st_top_publishers retrieves publisher data", {
  skip_on_cran()
  
  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token == "") {
    skip("Authentication token not found, skipping tests")
  }
  
  # Test basic functionality
  publishers <- st_top_publishers(
    measure = "revenue",
    os = "unified",
    category = 6014,  # Games
    time_range = "month",
    date = "2024-01-01",
    country = "US",
    limit = 5,
    auth_token = auth_token
  )
  
  expect_s3_class(publishers, "tbl_df")
  expect_true(nrow(publishers) > 0)
  expect_true(nrow(publishers) <= 5)
  
  # Check required columns exist
  expect_true("publisher_id" %in% names(publishers))
  expect_true("publisher_name" %in% names(publishers))
  expect_true("revenue_absolute" %in% names(publishers))
  expect_true("revenue_usd" %in% names(publishers))
  expect_true("rank" %in% names(publishers))
  
  # Check revenue conversion
  if (nrow(publishers) > 0) {
    expect_equal(publishers$revenue_usd[1], publishers$revenue_absolute[1] / 100)
  }
  
  # Test with downloads measure
  publishers_downloads <- st_top_publishers(
    measure = "units",
    os = "ios",
    category = 6000,  # All categories
    time_range = "week",
    date = "2024-01-01",
    country = "US",
    limit = 3,
    auth_token = auth_token
  )
  
  expect_s3_class(publishers_downloads, "tbl_df")
  expect_true("units_absolute" %in% names(publishers_downloads))
})

test_that("st_top_publishers handles different parameters", {
  skip_on_cran()
  
  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token == "") {
    skip("Authentication token not found, skipping tests")
  }
  
  # Test with growth metrics
  publishers_growth <- st_top_publishers(
    measure = "revenue",
    comparison_attribute = "delta",
    os = "android",
    category = "game",
    time_range = "month",
    date = "2024-01-01",
    country = "US",
    limit = 5,
    auth_token = auth_token
  )
  
  expect_s3_class(publishers_growth, "tbl_df")
  expect_true("revenue_delta" %in% names(publishers_growth))
  
  # Test without apps
  publishers_no_apps <- st_top_publishers(
    measure = "revenue",
    os = "unified",
    category = 6014,
    time_range = "month",
    date = "2024-01-01",
    country = "US",
    include_apps = FALSE,
    limit = 3,
    auth_token = auth_token
  )
  
  expect_false("apps" %in% names(publishers_no_apps))
})

test_that("st_top_publishers validates input", {
  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token == "") {
    skip("Authentication token not found, skipping tests")
  }
  
  # Test invalid measure
  expect_error(
    st_top_publishers(
      measure = "invalid",
      os = "unified",
      date = "2024-01-01",
      country = "US",
      auth_token = auth_token
    ),
    "should be one of"
  )
  
  # Test invalid OS
  expect_error(
    st_top_publishers(
      os = "windows",
      date = "2024-01-01",
      country = "US",
      auth_token = auth_token
    ),
    "should be one of"
  )
  
  # Test invalid limit
  expect_error(
    st_top_publishers(
      limit = 11,
      os = "unified",
      date = "2024-01-01",
      country = "US",
      auth_token = auth_token
    ),
    "limit must be between"
  )
  
  # Test missing auth token
  expect_error(
    st_top_publishers(
      os = "unified",
      date = "2024-01-01",
      country = "US",
      auth_token = ""
    ),
    "Authentication token is required"
  )
})

test_that("st_publisher_category_breakdown has been removed", {
  skip("st_publisher_category_breakdown has been removed from the package")
})