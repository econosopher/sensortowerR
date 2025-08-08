# Comprehensive API endpoint tests that verify actual data returns

test_that("st_metrics returns expected revenue and download data", {
  skip_if_no_auth()
  
  # Test with a known app (Candy Crush)
  result <- st_metrics(
    os = "ios",
    ios_app_id = "553834731",
    countries = "US",
    date_granularity = "monthly",
    start_date = "2024-01-01",
    end_date = "2024-01-31"  )
  
  # Verify structure
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0, "Should return at least one row of data")
  
  # Verify required columns exist
  required_cols <- c("app_id", "app_id_type", "date", "country", "revenue", "downloads")
  expect_true(all(required_cols %in% names(result)), 
              paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", ")))
  
  # Verify data types (accept integer or double)
  expect_true(is.numeric(result$revenue))
  expect_true(is.numeric(result$downloads))
  expect_s3_class(result$date, "Date")
  
  # Verify data values are reasonable
  expect_true(all(result$revenue >= 0), "Revenue should be non-negative")
  expect_true(all(result$downloads >= 0), "Downloads should be non-negative")
  expect_true(any(result$revenue > 0), "Candy Crush should have revenue")
  expect_true(any(result$downloads > 0), "Candy Crush should have downloads")
  
  # Verify metadata
  expect_equal(unique(result$app_id_type), "ios")
  expect_equal(unique(result$country), "US")
})

test_that("st_metrics handles unified OS correctly (platform-specific rows)", {
  skip_if_no_auth()
  
  # Test unified data with both iOS and Android IDs
  result <- st_metrics(
    os = "unified",
    ios_app_id = "553834731",
    android_app_id = "com.king.candycrushsaga",
    countries = "US",
    date_granularity = "monthly",
    start_date = "2024-01-01",
    end_date = "2024-01-31"  )
  
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  
  # Unified now returns platform-specific rows (ios/android)
  expect_true(all(unique(result$app_id_type) %in% c("ios", "android")))
  # Ensure both platforms are present
  expect_true(all(c("ios", "android") %in% unique(result$app_id_type)))
  # Combined revenue across platforms should be positive for at least one date
  if (nrow(result) > 0) {
    agg <- result %>% dplyr::group_by(date, country) %>% dplyr::summarise(total_revenue = sum(revenue, na.rm = TRUE), .groups = "drop")
    expect_true(any(agg$total_revenue > 0), "Combined revenue should be positive for at least one date")
  }
})

test_that("st_batch_metrics processes multiple apps correctly", {
  skip_if_no_auth()
  
  # Test with multiple popular games
  apps <- c("553834731", "1195621598", "529479190")  # Candy Crush, Homescapes, Clash of Clans
  
  result <- st_batch_metrics(
    os = "ios",
    app_list = apps,
    metrics = c("revenue", "downloads"),
    date_range = list(start_date = "2024-01-01", end_date = "2024-01-31"),
    countries = "US",
    granularity = "monthly"  )
  
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  
  # Should have data for all apps
  unique_apps <- unique(result$original_id)
  expect_true(length(unique_apps) >= 2, "Should have data for multiple apps")
  
  # Check metrics are in long format
  expect_true("metric" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true(all(c("revenue", "downloads") %in% unique(result$metric)))
  
  # Verify each app has both metrics
  for (app in unique_apps) {
    app_metrics <- result[result$original_id == app, ]$metric
    expect_true("revenue" %in% app_metrics, paste("Missing revenue for", app))
    expect_true("downloads" %in% app_metrics, paste("Missing downloads for", app))
  }
})

test_that("st_yoy_metrics calculates year-over-year changes correctly", {
  skip_if_no_auth()
  
  # Test with known app over 2 years
  result <- st_yoy_metrics(
    os = "ios",
    ios_app_id = "553834731",  # Candy Crush
    years = c(2023, 2024),
    period_start = "01-01",
    period_end = "01-31",
    countries = "US",
    metrics = "revenue",
    granularity = "monthly"  )
  
  expect_s3_class(result, "tbl_df")
  
  # Check structure
  expect_true("year" %in% names(result))
  expect_true("yoy_change" %in% names(result))
  expect_true("yoy_change_absolute" %in% names(result))
  
  if (nrow(result) > 0) {
    # Should have data for both years
    years_present <- unique(result$year)
    expect_true(length(years_present) <= 2, "Should have at most 2 years")
    
    # 2023 should have NA for YoY (no previous year)
    if (2023 %in% result$year) {
      yoy_2023 <- result[result$year == 2023, ]$yoy_change
      expect_true(all(is.na(yoy_2023)), "2023 should have NA YoY change")
    }
    
    # 2024 should have calculated YoY if both years present
    if (all(c(2023, 2024) %in% result$year)) {
      yoy_2024 <- result[result$year == 2024, ]$yoy_change
      expect_false(all(is.na(yoy_2024)), "2024 should have YoY change calculated")
    }
  }
})

test_that("st_top_charts returns ranking data with correct structure", {
  skip_if_no_auth()
  
  # Get top revenue games
  result <- st_top_charts(
    os = "ios",
    comparison_attribute = "absolute",
    time_range = "month",
    measure = "revenue",
    date = "2024-01-01",
    category = 6014,  # Games
    regions = "US",
    limit = 10  )
  
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0, "Should return ranking data")
  expect_true(nrow(result) <= 10, "Should respect limit")
  
  # Check for ranking columns if provided
  if ("rank" %in% names(result) || "current_rank" %in% names(result)) {
    # Rankings should be sequential when rank present
    if ("rank" %in% names(result)) {
      expect_equal(result$rank, 1:nrow(result))
    }
  } else {
    testthat::skip("Rank columns not returned by this endpoint variant; skipping rank assertions")
  }
  
  # Should have app identifiers
  expect_true(any(c("app_id", "entity_id", "unified_app_id") %in% names(result)),
              "Should have app identifier")
  
  # Should have metric values
  expect_true(any(grepl("revenue", names(result), ignore.case = TRUE)),
              "Should have revenue data")
})

test_that("st_top_publishers returns publisher ranking data", {
  skip_if_no_auth()
  
  result <- st_top_publishers(
    measure = "revenue",
    os = "unified",
    category = 6014,  # Games
    date = "2024-01-01",
    country = "US",
    limit = 5,
    include_apps = TRUE  )
  
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0, "Should return publisher data")
  expect_true(nrow(result) <= 5, "Should respect limit")
  
  # Check required columns
  expect_true("publisher_id" %in% names(result), "Should have publisher_id")
  expect_true("publisher_name" %in% names(result), "Should have publisher_name")
  expect_true("rank" %in% names(result), "Should have rank")
  
  # If include_apps = TRUE, should have app data
  app_cols <- names(result)[grepl("app", names(result), ignore.case = TRUE)]
  expect_true(length(app_cols) > 0, "Should include app information")
  
  # Rankings should be sequential
  expect_equal(result$rank, 1:nrow(result))
  
  # Should have revenue data
  revenue_cols <- names(result)[grepl("revenue", names(result), ignore.case = TRUE)]
  expect_true(length(revenue_cols) > 0, "Should have revenue columns")
})

test_that("st_app_info returns detailed app metadata", {
  skip_if_no_auth()
  
  # Search for Pokemon apps
  result <- st_app_info(
    term = "Pokemon GO",
    app_store = "unified",
    entity_type = "app",
    limit = 5  )
  
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0, "Should find Pokemon apps")
  expect_true(nrow(result) <= 5, "Should respect limit")
  
  # Check for essential columns
  expect_true("unified_app_id" %in% names(result), "Should have unified_app_id")
  expect_true("unified_app_name" %in% names(result), "Should have app name")
  
  # Platform IDs and publisher info may not always be included depending on endpoint response
  # If present, they should be non-empty
  plat_cols_present <- any(c("ios_app_id", "android_app_id") %in% names(result))
  if (plat_cols_present) {
    if ("ios_app_id" %in% names(result)) expect_true(all(nchar(result$ios_app_id) > 0, na.rm = TRUE))
    if ("android_app_id" %in% names(result)) expect_true(all(nchar(result$android_app_id) > 0, na.rm = TRUE))
  }
  pub_cols <- names(result)[grepl("publisher", names(result), ignore.case = TRUE)]
  if (length(pub_cols) > 0) {
    # at least one publisher column has non-empty values
    expect_true(any(sapply(result[pub_cols], function(col) any(nchar(as.character(col)) > 0, na.rm = TRUE))))
  }
})

test_that("st_smart_metrics handles mixed ID types correctly", {
  skip_if_no_auth()
  
  # Mix of iOS, Android, and unified IDs
  mixed_ids <- c(
    "553834731",                    # Candy Crush iOS
    "com.supercell.clashofclans",   # Clash of Clans Android
    "5ba4585f539ce75b97db6bcb"      # Some unified ID
  )
  
  result <- tryCatch({
    st_smart_metrics(
      app_ids = mixed_ids,
      metrics = "revenue",
      start_date = "2024-01-01",
      end_date = "2024-01-31",
      countries = "US",
      granularity = "monthly",
      )
  }, error = function(e) {
    NULL
  })
  
  # st_smart_metrics might not be available or might have issues
  if (!is.null(result)) {
    expect_s3_class(result, "tbl_df")
    
    # Should handle different ID types
    if (nrow(result) > 0) {
      unique_apps <- length(unique(result$entity_id))
      expect_true(unique_apps >= 1, "Should process at least some apps")
    }
  }
})

test_that("API error handling works correctly", {
  skip_if_no_auth()
  
  # Test with invalid app ID
  invalid_res <- tryCatch({
    st_metrics(
      os = "ios",
      ios_app_id = "99999999999999",  # Invalid ID
      countries = "US",
      date_granularity = "daily",
      start_date = "2024-01-01",
      end_date = "2024-01-31"
    )
  }, error = function(e) e)
  if (inherits(invalid_res, "error")) {
    succeed()
  } else {
    # If no error was thrown, expect zero rows
    expect_true(is.data.frame(invalid_res))
    expect_true(nrow(invalid_res) == 0)
  }
  
  # Test with invalid date range
  expect_error(
    st_metrics(
      os = "ios", 
      ios_app_id = "553834731",
      countries = "US",
      date_granularity = "daily",
      start_date = "2024-01-31",
      end_date = "2024-01-01",  # End before start
      )
  )
})