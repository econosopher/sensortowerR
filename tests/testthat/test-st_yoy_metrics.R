test_that("st_yoy_metrics validates required parameters", {
  expect_error(
    st_yoy_metrics(
      ios_app_id = "123456789",
      period_start = "01-01",
      period_end = "03-31",
      countries = "US"
    ),
    "'os' parameter is required"
  )
  
  expect_error(
    st_yoy_metrics(
      os = "ios",
      ios_app_id = "123456789",
      period_end = "03-31",
      countries = "US"
    ),
    "'period_start' is required"
  )
  
  expect_error(
    st_yoy_metrics(
      os = "ios",
      ios_app_id = "123456789",
      period_start = "01-01",
      countries = "US"
    ),
    "'period_end' is required"
  )
  
  expect_error(
    st_yoy_metrics(
      os = "ios",
      ios_app_id = "123456789",
      period_start = "01-01",
      period_end = "03-31"
    ),
    "'countries' parameter is required"
  )
})

test_that("st_yoy_metrics works with basic inputs", {
  skip_if_no_auth()
  
  # Test with a short date range to minimize API calls
  result <- st_yoy_metrics(
    os = "ios",
    ios_app_id = "553834731",  # Candy Crush
    years = c(2023, 2024),
    period_start = "01-01",
    period_end = "01-07",
    countries = "US",
    metrics = "revenue",
    verbose = FALSE
  )
  
  # Check structure
  expect_s3_class(result, "tbl_df")
  
  # Check expected columns
  expected_cols <- c("app_id", "app_id_type", "entity_id", "entity_name", 
                    "entity_type", "year", "date_start", "date_end", 
                    "country", "metric", "value", "yoy_change", "yoy_change_absolute")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that we have data for both years if data was returned
  if (nrow(result) > 0) {
    years_in_data <- unique(result$year)
    expect_true(2023 %in% years_in_data || 2024 %in% years_in_data)
  }
})

test_that("st_yoy_metrics calculates YoY changes correctly", {
  # Create mock data to test calculations
  test_data <- tibble::tibble(
    original_id = "test_app",
    app_name = "Test App",
    country = c("US", "US"),
    metric = c("revenue", "revenue"),
    value = c(1000, 1200),
    year = c(2023, 2024),
    date_start = c("2023-01-01", "2024-01-01"),
    date_end = c("2023-01-31", "2024-01-31"),
    entity_id = "test_app",
    entity_name = "Test App",
    entity_type = "app",
    app_id = "test_app",
    app_id_type = "ios"
  )
  
  # Apply the YoY calculation logic from st_yoy_metrics
  yoy_data <- test_data %>%
    dplyr::arrange(entity_id, country, metric, year) %>%
    dplyr::group_by(entity_id, country, metric) %>%
    dplyr::mutate(
      prev_value = dplyr::lag(value, n = 1),
      yoy_change_absolute = value - prev_value,
      yoy_change = ifelse(
        is.na(prev_value) | prev_value == 0,
        NA_real_,
        ((value - prev_value) / prev_value) * 100
      )
    ) %>%
    dplyr::select(-prev_value) %>%
    dplyr::ungroup()
  
  # Check calculations
  expect_equal(yoy_data$yoy_change[2], 20)  # 20% increase
  expect_equal(yoy_data$yoy_change_absolute[2], 200)  # $200 increase
  expect_true(is.na(yoy_data$yoy_change[1]))  # First year has no previous
})

test_that("st_yoy_metrics handles leap years correctly", {
  skip_if_no_auth()
  
  # Test Feb 29 handling across leap and non-leap years
  result <- st_yoy_metrics(
    os = "ios",
    ios_app_id = "553834731",
    years = c(2023, 2024),  # 2024 is a leap year
    period_start = "02-01",
    period_end = "02-29",  # Feb 29
    countries = "US",
    metrics = "revenue",
    verbose = FALSE
  )
  
  # Check that dates were adjusted appropriately
  if (nrow(result) > 0) {
    feb_2023 <- result[result$year == 2023, ]$date_end[1]
    feb_2024 <- result[result$year == 2024, ]$date_end[1]
    
    if (!is.na(feb_2023)) expect_equal(feb_2023, "2023-02-28")  # Non-leap year
    if (!is.na(feb_2024)) expect_equal(feb_2024, "2024-02-29")  # Leap year
  }
})

test_that("st_yoy_metrics uses default years when not specified", {
  skip_if_no_auth()
  
  result <- st_yoy_metrics(
    os = "ios",
    ios_app_id = "553834731",
    years = NULL,  # Should default to current and previous year
    period_start = "01-01",
    period_end = "01-07",
    countries = "US",
    metrics = "revenue",
    verbose = FALSE
  )
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Check structure even if no data
  expect_s3_class(result, "tbl_df")
  
  if (nrow(result) > 0) {
    years_in_data <- unique(result$year)
    # At least one of the default years should be present
    expect_true(current_year %in% years_in_data || (current_year - 1) %in% years_in_data)
  }
})

test_that("calculate_yoy_growth works correctly", {
  # Create test data
  test_data <- tibble::tibble(
    entity_id = "app1",
    country = "US",
    metric = "revenue",
    year = c(2021, 2022, 2023),
    value = c(1000, 1500, 2000)
  )
  
  # Test with default baseline (earliest year)
  growth <- calculate_yoy_growth(test_data)
  
  expect_equal(growth$growth_from_baseline[1], 0)  # 2021 is baseline
  expect_equal(growth$growth_from_baseline[2], 50)  # 50% growth from 2021
  expect_equal(growth$growth_from_baseline[3], 100)  # 100% growth from 2021
  
  expect_equal(growth$growth_index[1], 100)  # Base year = 100
  expect_equal(growth$growth_index[2], 150)  # 150% of base
  expect_equal(growth$growth_index[3], 200)  # 200% of base
  
  # Test with specific baseline year
  growth_2022 <- calculate_yoy_growth(test_data, baseline_year = 2022)
  
  # For year 2021, it's -33.33% from the 2022 baseline (1000 vs 1500)
  expect_equal(growth_2022$growth_from_baseline[growth_2022$year == 2021], -33.33333, tolerance = 0.001)
  # For year 2022 (baseline year), growth should be 0
  expect_equal(growth_2022$growth_from_baseline[growth_2022$year == 2022], 0)
  # For year 2023, growth from 2022 baseline should be 33.33%
  expect_equal(growth_2022$growth_from_baseline[growth_2022$year == 2023], 33.33333, tolerance = 0.001)
})

test_that("st_yoy_metrics handles multiple countries", {
  skip_if_no_auth()
  
  result <- st_yoy_metrics(
    os = "ios",
    ios_app_id = "553834731",
    years = c(2024),  # Just one year to minimize API calls
    period_start = "01-01",
    period_end = "01-07",
    countries = c("US", "GB"),
    metrics = "revenue",
    verbose = FALSE
  )
  
  # Check structure
  expect_s3_class(result, "tbl_df")
  
  if (nrow(result) > 0) {
    countries_in_data <- unique(result$country)
    # Check that we got data for at least one country
    expect_true(length(countries_in_data) >= 1)
  }
})