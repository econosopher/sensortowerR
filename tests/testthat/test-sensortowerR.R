test_that("API calls retrieve data", {
  skip_on_cran()

  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token == "") {
    skip("Authentication token not found, skipping tests")
  }

  # Test st_app_info
  app_info <- st_app_info(
    term = "Pokemon",
    app_store = "unified",
    entity_type = "app",
    limit = 1,
    auth_token = auth_token
  )
  expect_s3_class(app_info, "tbl_df")
  expect_true(nrow(app_info) > 0)

  # Test st_publisher_apps
  publisher_apps <- st_publisher_apps(
    auth_token = auth_token,
    publisher_id = "560c48b48ac350643900b82d" # Supercell
  )
  expect_s3_class(publisher_apps, "tbl_df")
  expect_true(nrow(publisher_apps) > 0)

  # Test st_metrics (updated to use new parameters)
  metrics <- st_metrics(
    os = "unified",
    app_id = "602c795c912b51622f233ffe", # Pokemon GO
    countries = "US",
    start_date = "2021-09-22",
    end_date = "2021-09-22",
    date_granularity = "daily",
    auth_token = auth_token
  )
  expect_s3_class(metrics, "tbl_df")
  # Note: May return 0 rows if daily data not available from unified endpoint

  # Test st_top_charts (replaces st_top_sales)
  top_sales <- st_top_charts(
    os = "ios",
    comparison_attribute = "absolute",
    time_range = "month",
    measure = "units",
    date = "2023-10-01",
    category = 6000,
    regions = "US",
    limit = 5,
    auth_token = auth_token
  )
  expect_s3_class(top_sales, "tbl_df")
  expect_true(nrow(top_sales) > 0)

  # Test st_top_charts with active users
  top_active <- st_top_charts(
    os = "android",
    comparison_attribute = "absolute",
    time_range = "quarter",
    measure = "MAU",
    date = "2023-10-01",
    category = "game",  # Games category for Android
    regions = "WW",
    limit = 3,
    auth_token = auth_token
  )
  expect_s3_class(top_active, "tbl_df")
  expect_true(nrow(top_active) > 0)
})

test_that("clean_numeric_values removes special characters correctly", {
  # Create test data with special characters in numeric columns
  test_data <- tibble::tibble(
    # Columns that should be cleaned
    downloads_180d_ww = c("1,234,567", "$2,500", "45%", "100.5", NA),
    revenue_30d_us = c("$1,000.50", "2,500%", "45", "", "N/A"),
    retention_7d_us = c("15.5%", "25%", "0", "45.2%", NA),
    aggregate_tags.test_metric = c("$100", "200%", "1,500", "", NA),
    
    # Columns that should NOT be cleaned (text data)
    app_name = c("Test App", "Another App", "Game", "Tool", "App"),
    category = c("Games", "Social", "Productivity", "Entertainment", "Utilities")
  )
  
  # Apply the cleaning function
  cleaned_data <- sensortowerR:::clean_numeric_values(test_data)
  
  # Check that numeric columns were converted to numeric type
  expect_true(is.numeric(cleaned_data$downloads_180d_ww))
  expect_true(is.numeric(cleaned_data$revenue_30d_us))
  expect_true(is.numeric(cleaned_data$retention_7d_us))
  expect_true(is.numeric(cleaned_data$`aggregate_tags.test_metric`))
  
  # Check that text columns remain as character
  expect_true(is.character(cleaned_data$app_name))
  expect_true(is.character(cleaned_data$category))
  
  # Check specific cleaned values
  expect_equal(cleaned_data$downloads_180d_ww[1], 1234567)
  expect_equal(cleaned_data$downloads_180d_ww[2], 2500)
  expect_equal(cleaned_data$downloads_180d_ww[3], 45)
  expect_equal(cleaned_data$revenue_30d_us[1], 1000.50)
  # Retention values are converted to decimals (divided by 100)
  expect_equal(cleaned_data$retention_7d_us[1], 0.155)
  expect_equal(cleaned_data$retention_7d_us[2], 0.25)
  
  # Check that NA values are preserved
  expect_true(is.na(cleaned_data$downloads_180d_ww[5]))
  expect_true(is.na(cleaned_data$retention_7d_us[5]))
})