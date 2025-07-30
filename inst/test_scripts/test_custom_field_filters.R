# Comprehensive Test Suite for Custom Field Filters
# This script tests the custom_fields_filter_id functionality across different scenarios

library(sensortowerR)
library(testthat)
library(dplyr)

# Test Configuration
TEST_FILTER_ID <- "687df26ac5a19ebcfe817d7f"  # Example from Sensor Tower support
VERBOSE <- TRUE

# Helper Functions ----

# Validate filter ID format
is_valid_filter_id <- function(id) {
  grepl("^[a-f0-9]{24}$", id)
}

# Safe API call wrapper
safe_api_call <- function(expr, context = "") {
  result <- tryCatch(
    expr,
    error = function(e) {
      list(
        success = FALSE,
        error = e$message,
        context = context
      )
    }
  )
  
  if (is.list(result) && !is.null(result$success) && !result$success) {
    return(result)
  }
  
  list(
    success = TRUE,
    data = result,
    context = context
  )
}

# Log test results
log_test <- function(test_name, result) {
  if (result$success) {
    cat("✅", test_name, "\n")
    if (!is.null(result$data) && is.data.frame(result$data)) {
      cat("   Returned", nrow(result$data), "rows\n")
    }
  } else {
    cat("❌", test_name, "\n")
    cat("   Error:", result$error, "\n")
  }
}

# Test Suite ----

cat("=== Custom Field Filter Test Suite ===\n")
cat("Test Filter ID:", TEST_FILTER_ID, "\n")
cat("Valid Format:", is_valid_filter_id(TEST_FILTER_ID), "\n\n")

# Test 1: Filter ID Format Validation
cat("## Test 1: Filter ID Format Validation\n")

test_ids <- list(
  valid_hex = "687df26ac5a19ebcfe817d7f",
  too_short = "687df26ac5a19ebcfe817d7",
  too_long = "687df26ac5a19ebcfe817d7ff",
  invalid_chars = "687df26ac5a19ebcfe817d7g",
  uppercase = "687DF26AC5A19EBCFE817D7F"
)

for (name in names(test_ids)) {
  id <- test_ids[[name]]
  valid <- is_valid_filter_id(id)
  cat(sprintf("  %s: %s - %s\n", name, id, ifelse(valid, "✅", "❌")))
}

cat("\n## Test 2: Basic Functionality Tests\n")

# Test 2.1: iOS with custom filter
result_ios <- safe_api_call(
  st_top_charts(
    os = "ios",
    measure = "revenue",
    custom_fields_filter_id = TEST_FILTER_ID,
    regions = "US",
    limit = 5
  ),
  "iOS with custom filter"
)
log_test("iOS with custom filter", result_ios)

# Test 2.2: Android with custom filter
result_android <- safe_api_call(
  st_top_charts(
    os = "android",
    measure = "revenue",
    custom_fields_filter_id = TEST_FILTER_ID,
    regions = "US",
    limit = 5
  ),
  "Android with custom filter"
)
log_test("Android with custom filter", result_android)

# Test 2.3: Unified with custom filter (requires custom_tags_mode)
result_unified <- safe_api_call(
  st_top_charts(
    os = "unified",
    measure = "revenue",
    custom_fields_filter_id = TEST_FILTER_ID,
    custom_tags_mode = "include",
    regions = "US",
    limit = 5
  ),
  "Unified with custom filter"
)
log_test("Unified with custom filter", result_unified)

# Test 2.4: Unified without custom_tags_mode (should fail)
result_unified_fail <- safe_api_call(
  st_top_charts(
    os = "unified",
    measure = "revenue",
    custom_fields_filter_id = TEST_FILTER_ID,
    regions = "US",
    limit = 5
  ),
  "Unified without custom_tags_mode"
)
log_test("Unified without custom_tags_mode (should fail)", result_unified_fail)

cat("\n## Test 3: Parameter Interaction Tests\n")

# Test 3.1: Custom filter without category
result_no_category <- safe_api_call(
  st_top_charts(
    os = "ios",
    measure = "revenue",
    custom_fields_filter_id = TEST_FILTER_ID,
    regions = "US",
    limit = 5
  ),
  "Custom filter without category"
)
log_test("Custom filter without category", result_no_category)

# Test 3.2: Custom filter with category (category should be ignored)
result_with_category <- safe_api_call(
  st_top_charts(
    os = "ios",
    measure = "revenue",
    category = 6014,  # Games
    custom_fields_filter_id = TEST_FILTER_ID,
    regions = "US",
    limit = 5
  ),
  "Custom filter with category"
)
log_test("Custom filter with category", result_with_category)

# Test 3.3: No filter and no category (should fail)
result_no_filter_no_category <- safe_api_call(
  st_top_charts(
    os = "ios",
    measure = "revenue",
    regions = "US",
    limit = 5
  ),
  "No filter and no category"
)
log_test("No filter and no category (should fail)", result_no_filter_no_category)

cat("\n## Test 4: Different Measures\n")

measures <- c("revenue", "units", "DAU", "WAU", "MAU")

for (measure in measures) {
  result <- safe_api_call(
    st_top_charts(
      os = "ios",
      measure = measure,
      custom_fields_filter_id = TEST_FILTER_ID,
      regions = "US",
      limit = 5
    ),
    paste("Measure:", measure)
  )
  log_test(paste("Measure:", measure), result)
}

cat("\n## Test 5: Custom Tags Mode Variations\n")

if (result_unified$success) {
  tags_modes <- c("include", "exclude", "include_unified_apps")
  
  for (mode in tags_modes) {
    result <- safe_api_call(
      st_top_charts(
        os = "unified",
        measure = "revenue",
        custom_fields_filter_id = TEST_FILTER_ID,
        custom_tags_mode = mode,
        regions = "US",
        limit = 5
      ),
      paste("Tags mode:", mode)
    )
    log_test(paste("Custom tags mode:", mode), result)
  }
}

cat("\n## Test 6: Time Range Variations\n")

time_ranges <- c("day", "week", "month", "quarter")

for (time_range in time_ranges) {
  result <- safe_api_call(
    st_top_charts(
      os = "ios",
      measure = "revenue",
      custom_fields_filter_id = TEST_FILTER_ID,
      regions = "US",
      time_range = time_range,
      limit = 5
    ),
    paste("Time range:", time_range)
  )
  log_test(paste("Time range:", time_range), result)
}

cat("\n## Test 7: Invalid Filter ID\n")

# Test with clearly invalid filter ID
result_invalid <- safe_api_call(
  st_top_charts(
    os = "ios",
    measure = "revenue",
    custom_fields_filter_id = "invalid_filter_id_12345",
    regions = "US",
    limit = 5
  ),
  "Invalid filter ID"
)
log_test("Invalid filter ID", result_invalid)

cat("\n## Test 8: Comparison Test\n")

# Compare results with and without filter to understand filter effect
if (result_ios$success) {
  # Get unfiltered data
  unfiltered <- safe_api_call(
    st_top_charts(
      os = "ios",
      measure = "revenue",
      category = 6014,  # Games
      regions = "US",
      limit = 20
    ),
    "Unfiltered comparison"
  )
  
  if (unfiltered$success && result_ios$success) {
    cat("\nComparison Results:\n")
    cat("  Unfiltered rows:", nrow(unfiltered$data), "\n")
    cat("  Filtered rows:", nrow(result_ios$data), "\n")
    
    # Check if filtered results are subset of unfiltered
    if (nrow(result_ios$data) > 0 && nrow(unfiltered$data) > 0) {
      if ("unified_app_id" %in% names(result_ios$data) && 
          "unified_app_id" %in% names(unfiltered$data)) {
        filtered_ids <- result_ios$data$unified_app_id
        unfiltered_ids <- unfiltered$data$unified_app_id
        
        is_subset <- all(filtered_ids %in% unfiltered_ids)
        cat("  Filtered is subset of unfiltered:", is_subset, "\n")
      }
    }
  }
}

# Summary Report ----

cat("\n=== Test Summary ===\n")

# Function to create test scenarios for documentation
generate_test_scenarios <- function() {
  scenarios <- list(
    list(
      name = "iOS Revenue with Custom Filter",
      code = 'st_top_charts(
  os = "ios",
  measure = "revenue",
  custom_fields_filter_id = "YOUR_FILTER_ID",
  regions = "US"
)'
    ),
    list(
      name = "Unified with Custom Tags Mode",
      code = 'st_top_charts(
  os = "unified",
  measure = "revenue",
  custom_fields_filter_id = "YOUR_FILTER_ID",
  custom_tags_mode = "include",
  regions = "US"
)'
    ),
    list(
      name = "Active Users with Custom Filter",
      code = 'st_top_charts(
  os = "android",
  measure = "MAU",
  custom_fields_filter_id = "YOUR_FILTER_ID",
  regions = "WW"
)'
    )
  )
  
  scenarios
}

# Generate example code snippets
cat("\n## Example Usage Patterns\n")
scenarios <- generate_test_scenarios()
for (scenario in scenarios) {
  cat("\n### ", scenario$name, "\n")
  cat("```r\n")
  cat(scenario$code, "\n")
  cat("```\n")
}

# Recommendations based on test results
cat("\n## Recommendations\n")

if (TEST_FILTER_ID == "687df26ac5a19ebcfe817d7f") {
  cat("⚠️  You're using the example filter ID from Sensor Tower support.\n")
  cat("   This ID may not work with your account.\n")
  cat("   To get a valid filter ID:\n")
  cat("   1. Log into app.sensortower.com\n")
  cat("   2. Configure filters in Top Apps section\n")
  cat("   3. Copy the custom_fields_filter_id from the URL\n")
}

# Save test results
test_results <- list(
  timestamp = Sys.time(),
  filter_id = TEST_FILTER_ID,
  tests = list(
    ios = result_ios,
    android = result_android,
    unified = result_unified,
    measures = measures,
    time_ranges = time_ranges
  )
)

saveRDS(test_results, "custom_filter_test_results.rds")
cat("\nTest results saved to custom_filter_test_results.rds\n")