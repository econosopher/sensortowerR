# Test which Sensor Tower endpoints accept multiple app IDs
# This will help us optimize API usage across the package

library(sensortowerR)
library(httr2)
library(jsonlite)
library(lubridate)
library(httr)

# Test configuration
test_ios_ids <- c("1195621598", "553834731")  # Homescapes, Candy Crush
test_android_ids <- c("com.playrix.homescapes", "com.king.candycrushsaga")
test_unified_ids <- c("5cd2e3bf5020ca0001426b23", "5704e5827e700bb6320004f2")
test_publisher_id <- "5614b8de3f07e25d29001b9e"  # Playrix

# Helper function to test endpoint
test_endpoint_batch <- function(endpoint, params, description) {
  cat(sprintf("\nTesting: %s\n", description))
  
  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  url <- paste0("https://api.sensortower.com/v1/", endpoint)
  
  # Test with single ID
  single_params <- params
  if ("app_ids" %in% names(params)) {
    single_params$app_ids <- strsplit(params$app_ids, ",")[[1]][1]
  }
  single_params$auth_token <- auth_token
  
  single_resp <- tryCatch({
    GET(url, query = single_params)
  }, error = function(e) NULL)
  
  # Test with multiple IDs
  multi_params <- params
  multi_params$auth_token <- auth_token
  
  multi_resp <- tryCatch({
    GET(url, query = multi_params)
  }, error = function(e) NULL)
  
  # Compare results
  single_status <- if (!is.null(single_resp)) status_code(single_resp) else "ERROR"
  multi_status <- if (!is.null(multi_resp)) status_code(multi_resp) else "ERROR"
  
  single_rows <- 0
  multi_rows <- 0
  
  if (single_status == 200) {
    content <- content(single_resp, as = "text", encoding = "UTF-8")
    if (nchar(content) > 0 && content != "[]") {
      data <- fromJSON(content, flatten = TRUE)
      single_rows <- if (is.data.frame(data)) nrow(data) else length(data)
    }
  }
  
  if (multi_status == 200) {
    content <- content(multi_resp, as = "text", encoding = "UTF-8")
    if (nchar(content) > 0 && content != "[]") {
      data <- fromJSON(content, flatten = TRUE)
      multi_rows <- if (is.data.frame(data)) nrow(data) else length(data)
    }
  }
  
  # Results
  cat(sprintf("  Single ID: Status %s, Rows: %d\n", single_status, single_rows))
  cat(sprintf("  Multi IDs: Status %s, Rows: %d\n", multi_status, multi_rows))
  
  if (multi_status == 200 && multi_rows > single_rows) {
    cat("  ✅ SUPPORTS BATCH CALLS!\n")
    return(TRUE)
  } else if (multi_status == 200 && multi_rows == single_rows) {
    cat("  ⚠️  May not support batch (same row count)\n")
    return(FALSE)
  } else {
    cat("  ❌ Does not support batch calls\n")
    return(FALSE)
  }
}

# Run tests
cat("=== SENSOR TOWER BATCH API TESTING ===\n")
cat("Testing which endpoints accept multiple app IDs...\n")

results <- list()

# 1. Sales Report (we know this works)
results$sales_ios <- test_endpoint_batch(
  "ios/sales_report_estimates",
  list(
    app_ids = paste(test_ios_ids, collapse = ","),
    countries = "US",
    start_date = as.character(Sys.Date() - 7),
    end_date = as.character(Sys.Date() - 1),
    date_granularity = "daily"
  ),
  "iOS Sales Report (known to work)"
)

# 2. App Details
results$app_details <- test_endpoint_batch(
  "ios/apps",
  list(app_ids = paste(test_ios_ids, collapse = ",")),
  "iOS App Details"
)

# 3. Usage/Active Users
results$usage <- test_endpoint_batch(
  "ios/usage/active_users",
  list(
    app_ids = paste(test_ios_ids, collapse = ","),
    time_period = "month",
    countries = "US"
  ),
  "iOS Active Users"
)

# 4. Category Rankings (probably doesn't support batch)
results$rankings <- test_endpoint_batch(
  "ios/category/category_rankings",
  list(
    app_ids = paste(test_ios_ids, collapse = ","),
    category = "6014",
    chart_type = "topfreeapplications",
    countries = "US"
  ),
  "iOS Category Rankings"
)

# 5. Unified endpoints
results$unified_sales <- test_endpoint_batch(
  "unified/sales_report_estimates",
  list(
    app_ids = paste(test_unified_ids, collapse = ","),
    date_granularity = "monthly",
    start_date = as.character(floor_date(Sys.Date() - 30, "month")),
    end_date = as.character(Sys.Date() - 1)
  ),
  "Unified Sales (monthly)"
)

# 6. Android endpoints
results$android_sales <- test_endpoint_batch(
  "android/sales_report_estimates",
  list(
    app_ids = paste(test_android_ids, collapse = ","),
    countries = "US",
    start_date = as.character(Sys.Date() - 7),
    end_date = as.character(Sys.Date() - 1),
    date_granularity = "daily"
  ),
  "Android Sales Report"
)

# Summary
cat("\n=== SUMMARY ===\n")
batch_supported <- names(results)[unlist(results)]
cat("Endpoints supporting batch calls:\n")
for (endpoint in batch_supported) {
  cat(sprintf("  ✅ %s\n", endpoint))
}

cat("\nRecommendations:\n")
cat("1. Use batch calls for st_sales_report() - ALREADY IMPLEMENTED\n")
cat("2. Consider batch calls for st_app_details() if supported\n")
cat("3. Update st_metrics() to use batch st_sales_report() when possible\n")
cat("4. Document batch capabilities in function help\n")

# Additional tests for functions that might benefit
cat("\n=== ADDITIONAL OPTIMIZATION OPPORTUNITIES ===\n")

# Check if st_app_info could batch
cat("\n1. st_app_info() - Currently searches by name, but could accept multiple IDs\n")

# Check if st_publisher_apps gets all apps in one call
cat("2. st_publisher_apps() - Already efficient (single call per publisher)\n")

# Check metrics endpoints
cat("3. st_metrics() - Already updated to use platform-specific batch calls\n")

# Save results
saveRDS(results, "batch_api_test_results.rds")
cat("\nResults saved to batch_api_test_results.rds\n")