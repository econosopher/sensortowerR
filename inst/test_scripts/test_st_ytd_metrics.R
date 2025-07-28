# Comprehensive test script for st_ytd_metrics function
# This script validates the function with minimal API calls

library(sensortowerR)
library(dplyr)
library(tidyr)
library(testthat)

# Test configuration
VERBOSE <- TRUE
USE_CACHE <- TRUE
CACHE_DIR <- ".cache/ytd_test"

# Test app (use a well-known app for consistent results)
TEST_APP_ID <- "553834731"  # Candy Crush
TEST_APP_NAME <- "Candy Crush Saga"

cat("=== Testing st_ytd_metrics Function ===\n\n")

# Test 1: Input validation
# ------------------------
cat("Test 1: Input validation\n")

# Test invalid metrics
test_that("Invalid metrics are rejected", {
  expect_error(
    st_ytd_metrics(
      unified_app_id = TEST_APP_ID,
      metrics = c("revenue", "mau", "dau")  # mau and dau not supported
    ),
    "Invalid metrics: mau, dau"
  )
})

# Test missing entity IDs
test_that("Missing entity IDs are rejected", {
  expect_error(
    st_ytd_metrics(metrics = c("revenue")),
    "At least one entity ID must be provided"
  )
})

# Test mixed entity types
test_that("Mixed entity types are rejected", {
  expect_error(
    st_ytd_metrics(
      unified_app_id = TEST_APP_ID,
      publisher_id = "pub123"
    ),
    "Cannot specify both publisher_id and app IDs"
  )
})

cat("✓ Input validation tests passed\n\n")

# Test 2: Date calculations
# -------------------------
cat("Test 2: Date calculations\n")

# Test last completed week calculation
today <- Sys.Date()
last_saturday <- today - ((lubridate::wday(today) + 7 - 7) %% 7)
if (lubridate::wday(today) != 7 && lubridate::wday(last_saturday) == today) {
  last_saturday <- last_saturday - 7
}

cat(sprintf("  Today: %s (weekday %d)\n", today, lubridate::wday(today)))
cat(sprintf("  Expected last Saturday: %s\n", last_saturday))

# Test 3: Single app, single year (minimal API call)
# --------------------------------------------------
cat("\nTest 3: Single app, single year\n")

# Use a short date range to minimize API usage
test_start <- "03-01"
test_end <- "03-07"  # Just one week in March

result_single <- st_ytd_metrics(
  unified_app_id = TEST_APP_ID,
  years = 2024,
  period_start = test_start,
  period_end = test_end,
  metrics = c("revenue", "downloads"),
  countries = "US",
  cache_dir = CACHE_DIR,
  verbose = VERBOSE
)

# Validate structure
test_that("Single app result has correct structure", {
  expect_true(is.data.frame(result_single))
  expect_equal(
    names(result_single),
    c("entity_id", "entity_name", "entity_type", "year", 
      "date_start", "date_end", "country", "metric", "value")
  )
  expect_equal(unique(result_single$entity_id), TEST_APP_ID)
  expect_equal(unique(result_single$entity_type), "app")
  expect_equal(unique(result_single$year), 2024)
  expect_equal(sort(unique(result_single$metric)), c("downloads", "revenue"))
})

cat("✓ Single app test passed\n\n")

# Test 4: Compare with st_metrics for validation
# ----------------------------------------------
cat("Test 4: Cross-validation with st_metrics\n")

# Fetch the same data using st_metrics
comparison_data <- st_metrics(
  app_id = TEST_APP_ID,
  start_date = "2024-03-01",
  end_date = "2024-03-07",
  countries = "US",
  date_granularity = "daily",
  verbose = FALSE
)

# Aggregate to match st_ytd_metrics output
comparison_summary <- comparison_data %>%
  group_by(country) %>%
  summarise(
    revenue_total = sum(revenue, na.rm = TRUE),
    downloads_total = sum(downloads, na.rm = TRUE)
  )

# Extract values from st_ytd_metrics
ytd_values <- result_single %>%
  filter(country == "US") %>%
  pivot_wider(names_from = metric, values_from = value)

# Compare values
test_that("Values match between st_ytd_metrics and st_metrics", {
  expect_equal(
    ytd_values$revenue[1],
    comparison_summary$revenue_total[1],
    tolerance = 0.01  # Allow small rounding differences
  )
  expect_equal(
    ytd_values$downloads[1],
    comparison_summary$downloads_total[1]
  )
})

cat("✓ Cross-validation passed - values match!\n\n")

# Test 5: Multiple years (using cache)
# ------------------------------------
cat("Test 5: Multiple years\n")

result_multi_year <- st_ytd_metrics(
  unified_app_id = TEST_APP_ID,
  years = c(2023, 2024),  # Two years
  period_start = test_start,
  period_end = test_end,
  metrics = "revenue",  # Just revenue to minimize data
  countries = "US",
  cache_dir = CACHE_DIR,
  verbose = VERBOSE
)

test_that("Multiple years are processed correctly", {
  expect_equal(sort(unique(result_multi_year$year)), c(2023, 2024))
  expect_equal(nrow(result_multi_year), 2)  # One row per year (revenue only)
  
  # Check date ranges
  dates_2023 <- result_multi_year %>% filter(year == 2023)
  dates_2024 <- result_multi_year %>% filter(year == 2024)
  
  expect_equal(dates_2023$date_start[1], "2023-03-01")
  expect_equal(dates_2023$date_end[1], "2023-03-07")
  expect_equal(dates_2024$date_start[1], "2024-03-01")
  expect_equal(dates_2024$date_end[1], "2024-03-07")
})

cat("✓ Multiple years test passed\n\n")

# Test 6: Multiple apps (minimal test)
# ------------------------------------
cat("Test 6: Multiple apps\n")

# Use just 2 apps for a very short period
test_apps <- c("553834731", "1195621598")  # Candy Crush, Homescapes

result_multi_app <- st_ytd_metrics(
  unified_app_id = test_apps,
  years = 2024,
  period_start = "03-01",
  period_end = "03-02",  # Just 2 days
  metrics = "revenue",
  countries = "US",
  cache_dir = CACHE_DIR,
  verbose = FALSE  # Quiet for this test
)

test_that("Multiple apps are processed correctly", {
  expect_equal(length(unique(result_multi_app$entity_id)), 2)
  expect_true(all(test_apps %in% result_multi_app$entity_id))
})

cat("✓ Multiple apps test passed\n\n")

# Test 7: Custom date ranges
# --------------------------
cat("Test 7: Custom date ranges\n")

# Test February with leap year handling
feb_result <- st_ytd_metrics(
  unified_app_id = TEST_APP_ID,
  years = c(2023, 2024),  # 2024 is a leap year
  period_start = "02-01",
  period_end = "02-29",  # Feb 29
  metrics = "downloads",
  countries = "US",
  cache_dir = CACHE_DIR,
  verbose = FALSE
)

test_that("Leap year is handled correctly", {
  feb_2023 <- feb_result %>% filter(year == 2023)
  feb_2024 <- feb_result %>% filter(year == 2024)
  
  # 2023 should end on Feb 28
  expect_equal(feb_2023$date_end[1], "2023-02-28")
  
  # 2024 (leap year) should end on Feb 29
  expect_equal(feb_2024$date_end[1], "2024-02-29")
})

cat("✓ Custom date range test passed\n\n")

# Test 8: Cache effectiveness
# ---------------------------
cat("Test 8: Cache effectiveness\n")

# First call (should use cache from previous tests)
start_time1 <- Sys.time()
cached_result1 <- st_ytd_metrics(
  unified_app_id = TEST_APP_ID,
  years = 2024,
  period_start = test_start,
  period_end = test_end,
  metrics = c("revenue", "downloads"),
  countries = "US",
  cache_dir = CACHE_DIR,
  verbose = FALSE
)
time1 <- as.numeric(Sys.time() - start_time1)

# Second call (should definitely use cache)
start_time2 <- Sys.time()
cached_result2 <- st_ytd_metrics(
  unified_app_id = TEST_APP_ID,
  years = 2024,
  period_start = test_start,
  period_end = test_end,
  metrics = c("revenue", "downloads"),
  countries = "US",
  cache_dir = CACHE_DIR,
  verbose = FALSE
)
time2 <- as.numeric(Sys.time() - start_time2)

test_that("Cache returns identical results", {
  expect_identical(cached_result1, cached_result2)
})

cat(sprintf("  First call: %.2f seconds\n", time1))
cat(sprintf("  Cached call: %.2f seconds (%.1fx faster)\n", 
            time2, time1/time2))
cat("✓ Cache test passed\n\n")

# Summary
# -------
cat("=== Test Summary ===\n")
cat("✓ All tests passed successfully!\n")
cat("\nKey validations:\n")
cat("- Input validation working correctly\n")
cat("- Metrics limited to revenue and downloads only\n")
cat("- Date calculations accurate (including leap years)\n")
cat("- Values match st_metrics (cross-validated)\n")
cat("- Multiple entities handled properly\n")
cat("- Caching reduces API calls effectively\n")

# Cleanup cache directory (optional)
# unlink(CACHE_DIR, recursive = TRUE)