# Comprehensive API Test Suite for sensortowerR
# This script tests all major functions to ensure they work correctly with the API
# Run this to verify package functionality after changes

library(sensortowerR)
library(dplyr)

# Initialize test environment
cat("\n=====================================\n")
cat("sensortowerR Comprehensive API Tests\n")
cat("Version:", as.character(packageVersion("sensortowerR")), "\n")
cat("Date:", Sys.Date(), "\n")
cat("=====================================\n\n")

# Check authentication
if (Sys.getenv("SENSORTOWER_AUTH_TOKEN") == "") {
  stop("Please set SENSORTOWER_AUTH_TOKEN environment variable")
}

# Helper function to test and report
test_function <- function(name, test_code, expected_cols = NULL) {
  cat("\n--- Testing", name, "---\n")
  result <- tryCatch({
    eval(test_code)
  }, error = function(e) {
    cat("✗ ERROR:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(result)) {
    cat("✓ Success: Retrieved", nrow(result), "rows,", ncol(result), "columns\n")
    
    # Validate expected columns if provided
    if (!is.null(expected_cols)) {
      missing_cols <- setdiff(expected_cols, names(result))
      if (length(missing_cols) > 0) {
        cat("⚠ Warning: Missing expected columns:", paste(missing_cols, collapse = ", "), "\n")
      }
    }
    
    # Show sample data
    if (nrow(result) > 0) {
      cat("\nSample data (first 3 rows, max 5 columns):\n")
      print(head(result[, 1:min(5, ncol(result))], 3))
    }
  }
  
  return(result)
}

# Store test results
test_results <- list()
test_summary <- data.frame(
  Function = character(),
  Status = character(),
  Rows = integer(),
  Time = numeric(),
  stringsAsFactors = FALSE
)

#############################################
# 1. Core Metrics Functions
#############################################

cat("\n========== CORE METRICS ==========\n")

# 1.1 st_metrics - iOS
start_time <- Sys.time()
test_results$st_metrics_ios <- test_function(
  "st_metrics (iOS)",
  quote({
    st_metrics(
      os = "ios",
      ios_app_id = "553834731",  # Candy Crush
      countries = "US",
      date_granularity = "monthly",
      start_date = "2024-01-01",
      end_date = "2024-03-31"
    )
  }),
  expected_cols = c("app_id", "app_id_type", "date", "country", "revenue", "downloads")
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_metrics (iOS)",
  Status = if(!is.null(test_results$st_metrics_ios)) "PASS" else "FAIL",
  Rows = if(!is.null(test_results$st_metrics_ios)) nrow(test_results$st_metrics_ios) else 0,
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

# 1.2 st_metrics - Unified
start_time <- Sys.time()
test_results$st_metrics_unified <- test_function(
  "st_metrics (Unified)",
  quote({
    st_metrics(
      os = "unified",
      ios_app_id = "553834731",
      android_app_id = "com.king.candycrushsaga",
      countries = "WW",
      date_granularity = "monthly",
      start_date = "2024-01-01",
      end_date = "2024-01-31"
    )
  })
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_metrics (Unified)",
  Status = if(!is.null(test_results$st_metrics_unified)) "PASS" else "FAIL",
  Rows = if(!is.null(test_results$st_metrics_unified)) nrow(test_results$st_metrics_unified) else 0,
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

#############################################
# 2. Batch Processing Functions
#############################################

cat("\n\n========== BATCH PROCESSING ==========\n")

# 2.1 st_batch_metrics - Revenue/Downloads
start_time <- Sys.time()
test_results$st_batch_metrics <- test_function(
  "st_batch_metrics (Revenue/Downloads)",
  quote({
    st_batch_metrics(
      os = "ios",
      app_list = c("553834731", "1195621598"),  # Candy Crush, Match 3D
      metrics = c("revenue", "downloads"),
      date_range = list(start_date = "2024-01-01", end_date = "2024-01-31"),
      countries = "US",
      granularity = "monthly",
      verbose = FALSE
    )
  }),
  expected_cols = c("original_id", "metric", "value")
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_batch_metrics",
  Status = if(!is.null(test_results$st_batch_metrics)) "PASS" else "FAIL",
  Rows = if(!is.null(test_results$st_batch_metrics)) nrow(test_results$st_batch_metrics) else 0,
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

# 2.2 st_yoy_metrics
start_time <- Sys.time()
test_results$st_yoy_metrics <- test_function(
  "st_yoy_metrics",
  quote({
    st_yoy_metrics(
      os = "ios",
      ios_app_id = "553834731",
      years = c(2023, 2024),
      period_start = "01-01",
      period_end = "03-31",
      countries = "US",
      metrics = c("revenue", "downloads"),
      verbose = FALSE
    )
  }),
  expected_cols = c("year", "metric", "value", "yoy_change")
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_yoy_metrics",
  Status = if(!is.null(test_results$st_yoy_metrics)) "PASS" else "FAIL",
  Rows = if(!is.null(test_results$st_yoy_metrics)) nrow(test_results$st_yoy_metrics) else 0,
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

#############################################
# 3. Market Intelligence Functions
#############################################

cat("\n\n========== MARKET INTELLIGENCE ==========\n")

# 3.1 st_top_charts
start_time <- Sys.time()
test_results$st_top_charts <- test_function(
  "st_top_charts",
  quote({
    st_top_charts(
      os = "ios",
      comparison_attribute = "absolute",
      time_range = "month",
      measure = "revenue",
      date = "2024-01-01",
      category = 6014,  # Games
      regions = "US",
      limit = 10
    )
  })
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_top_charts",
  Status = if(!is.null(test_results$st_top_charts)) "PASS" else "FAIL",
  Rows = if(!is.null(test_results$st_top_charts)) nrow(test_results$st_top_charts) else 0,
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

# 3.2 st_top_publishers
start_time <- Sys.time()
test_results$st_top_publishers <- test_function(
  "st_top_publishers",
  quote({
    st_top_publishers(
      measure = "revenue",
      os = "unified",
      category = 6014,
      date = "2024-01-01",
      country = "US",
      limit = 5,
      include_apps = FALSE
    )
  })
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_top_publishers",
  Status = if(!is.null(test_results$st_top_publishers)) "PASS" else "FAIL",
  Rows = if(!is.null(test_results$st_top_publishers)) nrow(test_results$st_top_publishers) else 0,
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

#############################################
# 4. App Information Functions
#############################################

cat("\n\n========== APP INFORMATION ==========\n")

# 4.1 st_app_info
start_time <- Sys.time()
test_results$st_app_info <- test_function(
  "st_app_info",
  quote({
    st_app_info(
      term = "Candy Crush",
      app_store = "unified",
      entity_type = "app",
      limit = 3
    )
  })
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_app_info",
  Status = if(!is.null(test_results$st_app_info)) "PASS" else "FAIL",
  Rows = if(!is.null(test_results$st_app_info)) nrow(test_results$st_app_info) else 0,
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

# 4.2 st_app_lookup
start_time <- Sys.time()
test_results$st_app_lookup <- test_function(
  "st_app_lookup",
  quote({
    st_app_lookup(
      unified_id = "55c5028802ac64f9c0001faf",  # Candy Crush Saga unified ID
      verbose = FALSE
    )
  })
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_app_lookup",
  Status = if(!is.null(test_results$st_app_lookup)) "PASS" else "FAIL",
  Rows = 1,  # Always returns 1 row
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

#############################################
# 5. Category Analysis Functions
#############################################

cat("\n\n========== CATEGORY ANALYSIS ==========\n")

# 5.1 st_game_summary
start_time <- Sys.time()
test_results$st_game_summary <- test_function(
  "st_game_summary",
  quote({
    st_game_summary(
      categories = 6014,  # Games
      countries = "US",
      os = "ios",
      date_granularity = "monthly",
      start_date = "2024-01-01",
      end_date = "2024-01-31"
    )
  })
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_game_summary",
  Status = if(!is.null(test_results$st_game_summary)) "PASS" else "FAIL",
  Rows = if(!is.null(test_results$st_game_summary)) nrow(test_results$st_game_summary) else 0,
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

# 5.2 st_category_rankings
start_time <- Sys.time()
test_results$st_category_rankings <- test_function(
  "st_category_rankings",
  quote({
    st_category_rankings(
      os = "ios",
      category = 6014,  # Games
      chart_type = "topfreeapplications",
      country = "US",
      date = "2024-01-01",
      limit = 10
    )
  })
)
test_summary <- rbind(test_summary, data.frame(
  Function = "st_category_rankings",
  Status = if(!is.null(test_results$st_category_rankings)) "PASS" else "FAIL",
  Rows = if(!is.null(test_results$st_category_rankings)) nrow(test_results$st_category_rankings) else 0,
  Time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
))

#############################################
# TEST SUMMARY
#############################################

cat("\n\n=====================================\n")
cat("TEST SUMMARY\n")
cat("=====================================\n\n")

# Overall statistics
total_tests <- nrow(test_summary)
passed_tests <- sum(test_summary$Status == "PASS")
failed_tests <- sum(test_summary$Status == "FAIL")
total_time <- sum(test_summary$Time)

cat("Total Tests:", total_tests, "\n")
cat("Passed:", passed_tests, "\n")
cat("Failed:", failed_tests, "\n")
cat("Total Time:", round(total_time, 2), "seconds\n\n")

# Detailed results
cat("Detailed Results:\n")
print(test_summary, row.names = FALSE)

# Save results
results_file <- paste0("API_TEST_RESULTS_", Sys.Date(), ".md")
cat("\n\nSaving detailed results to:", results_file, "\n")

# Generate markdown report
report <- c(
  paste("# sensortowerR API Test Results -", Sys.Date()),
  "",
  "## Summary",
  paste("- **Version**:", packageVersion("sensortowerR")),
  paste("- **Tests Run**:", total_tests),
  paste("- **Passed**:", passed_tests),
  paste("- **Failed**:", failed_tests),
  paste("- **Total Time**:", round(total_time, 2), "seconds"),
  "",
  "## Detailed Results",
  "",
  "| Function | Status | Rows | Time (s) |",
  "|----------|--------|------|----------|"
)

for (i in 1:nrow(test_summary)) {
  report <- c(report, paste("|", 
    test_summary$Function[i], "|",
    test_summary$Status[i], "|",
    test_summary$Rows[i], "|",
    round(test_summary$Time[i], 3), "|"
  ))
}

# Add data validation notes
report <- c(report,
  "",
  "## Data Validation",
  "",
  "### Revenue Data",
  if (!is.null(test_results$st_metrics_ios)) {
    paste("- iOS Candy Crush Jan 2024 Revenue: $", 
          format(test_results$st_metrics_ios$revenue[1], big.mark = ","))
  } else {
    "- Unable to validate revenue data"
  },
  "",
  "### Active Users",
  "See `test-active-users.R` for comprehensive active user metrics testing.",
  "",
  "### Notes",
  "- All monetary values are in dollars (not cents)",
  "- Date ranges are inclusive",
  "- Country code 'WW' represents worldwide data"
)

writeLines(report, results_file)

cat("\n✓ Test suite completed!\n")