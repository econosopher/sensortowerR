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
    st_top_publishers(measure = "invalid", auth_token = auth_token),
    "should be one of"
  )
  
  # Test invalid OS
  expect_error(
    st_top_publishers(os = "windows", auth_token = auth_token),
    "should be one of"
  )
  
  # Test invalid limit
  expect_error(
    st_top_publishers(limit = 150, auth_token = auth_token),
    "limit must be between"
  )
  
  # Test missing auth token
  expect_error(
    st_top_publishers(auth_token = ""),
    "Authentication token is required"
  )
})

test_that("st_publisher_category_breakdown works correctly", {
  skip_on_cran()
  
  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token == "") {
    skip("Authentication token not found, skipping tests")
  }
  
  # First get some publishers
  publishers <- st_top_publishers(
    measure = "revenue",
    os = "unified", 
    category = 6014,
    limit = 2,
    auth_token = auth_token
  )
  
  if (nrow(publishers) == 0) {
    skip("No publishers returned from API")
  }
  
  # Test category breakdown
  breakdown <- st_publisher_category_breakdown(
    publisher_ids = publishers$publisher_id[1:2],
    time_range = "month",
    os = "unified",
    auth_token = auth_token
  )
  
  expect_s3_class(breakdown, "tbl_df")
  
  # Check columns if data returned
  if (nrow(breakdown) > 0) {
    expect_true("publisher_id" %in% names(breakdown))
    expect_true("publisher_name" %in% names(breakdown))
    expect_true("category_id" %in% names(breakdown))
    expect_true("revenue_absolute" %in% names(breakdown))
    expect_true("revenue_usd" %in% names(breakdown))
    expect_true("category_percentage" %in% names(breakdown))
    
    # Check percentages sum to ~100 for each publisher
    publisher_totals <- breakdown %>%
      group_by(publisher_id) %>%
      summarise(total_pct = sum(category_percentage, na.rm = TRUE))
    
    # Allow some tolerance due to rounding
    expect_true(all(abs(publisher_totals$total_pct - 100) < 5))
  }
})