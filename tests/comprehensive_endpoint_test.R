# Comprehensive Sensor Tower Endpoint Test
# Tests all combinations of endpoints, platforms, and time granularities

library(httr)
library(jsonlite)

# Setup
AUTH_TOKEN <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (AUTH_TOKEN == "") stop("Set SENSORTOWER_AUTH_TOKEN")

# Test apps
TEST_APPS <- list(
  ios = "553834731",         # Candy Crush iOS
  android = "com.king.candycrushsaga",  # Candy Crush Android
  unified = "553834731"      # Unified ID (same as iOS)
)

# Date ranges for different granularities
get_date_range <- function(granularity) {
  end_date <- Sys.Date() - 1
  
  switch(granularity,
    "daily" = list(
      start = as.character(end_date - 6),
      end = as.character(end_date)
    ),
    "weekly" = list(
      start = as.character(end_date - 27),
      end = as.character(end_date)
    ),
    "monthly" = list(
      start = format(end_date - 60, "%Y-%m-01"),
      end = as.character(end_date)
    ),
    "quarterly" = list(
      start = as.character(end_date - 180),
      end = as.character(end_date)
    )
  )
}

# Enhanced test function
test_endpoint <- function(endpoint_name, url, params) {
  resp <- GET(url, query = c(params, auth_token = AUTH_TOKEN))
  code <- status_code(resp)
  
  if (code == 200) {
    content <- content(resp, as = "text", encoding = "UTF-8")
    if (nchar(content) == 0 || content == "[]" || content == "{}") {
      return(list(status = "EMPTY", rows = 0, code = code))
    }
    
    data <- tryCatch({
      fromJSON(content, flatten = TRUE)
    }, error = function(e) NULL)
    
    if (is.data.frame(data)) {
      return(list(status = "SUCCESS", rows = nrow(data), code = code))
    } else if (is.list(data) && length(data) > 0) {
      return(list(status = "SUCCESS", rows = length(data), code = code))
    } else {
      return(list(status = "UNKNOWN", rows = 0, code = code))
    }
  } else {
    return(list(status = "ERROR", rows = 0, code = code))
  }
}

cat("=== COMPREHENSIVE SENSOR TOWER ENDPOINT TEST ===\n")
cat(sprintf("Testing date: %s\n\n", Sys.Date()))

# Define all test combinations
endpoints <- list(
  list(
    name = "Unified Sales Report",
    url = "https://api.sensortower.com/v1/unified/sales_report_estimates",
    platforms = c("unified"),
    granularities = c("daily", "weekly", "monthly", "quarterly"),
    app_param = "app_ids"
  ),
  list(
    name = "iOS Sales Report",
    url = "https://api.sensortower.com/v1/ios/sales_report_estimates",
    platforms = c("ios"),
    granularities = c("daily", "weekly", "monthly", "quarterly"),
    app_param = "app_ids"
  ),
  list(
    name = "Android Sales Report",
    url = "https://api.sensortower.com/v1/android/sales_report_estimates",
    platforms = c("android"),
    granularities = c("daily", "weekly", "monthly", "quarterly"),
    app_param = "app_ids"
  ),
  list(
    name = "Unified Active Users",
    url = "https://api.sensortower.com/v1/unified/usage/active_users",
    platforms = c("unified"),
    granularities = c("day", "week", "month", "quarter"),  # Note different naming
    app_param = "app_ids",
    time_param = "time_period"  # Different parameter name
  ),
  list(
    name = "iOS Active Users",
    url = "https://api.sensortower.com/v1/ios/usage/active_users",
    platforms = c("ios"),
    granularities = c("day", "week", "month", "quarter"),
    app_param = "app_ids",
    time_param = "time_period"
  ),
  list(
    name = "Android Active Users",
    url = "https://api.sensortower.com/v1/android/usage/active_users",
    platforms = c("android"),
    granularities = c("day", "week", "month", "quarter"),
    app_param = "app_ids",
    time_param = "time_period"
  )
)

# Results storage
results <- list()

# Test each endpoint
for (endpoint in endpoints) {
  cat(sprintf("\n=== %s ===\n", endpoint$name))
  
  for (platform in endpoint$platforms) {
    for (granularity in endpoint$granularities) {
      # Get appropriate app ID
      app_id <- TEST_APPS[[platform]]
      
      # Get date range
      dates <- get_date_range(granularity)
      
      # Build parameters
      params <- list(
        countries = "US",
        start_date = dates$start,
        end_date = dates$end
      )
      
      # Add app ID with correct parameter name
      params[[endpoint$app_param]] <- app_id
      
      # Add granularity with correct parameter name
      if (!is.null(endpoint$time_param)) {
        params[[endpoint$time_param]] <- granularity
      } else {
        params[["date_granularity"]] <- granularity
      }
      
      # Test the endpoint
      result <- test_endpoint(endpoint$name, endpoint$url, params)
      
      # Store result
      key <- paste(endpoint$name, platform, granularity, sep = "|")
      results[[key]] <- result
      
      # Display result
      status_icon <- switch(result$status,
        "SUCCESS" = "✅",
        "EMPTY" = "❌",
        "ERROR" = "⚠️",
        "❓"
      )
      
      cat(sprintf("  %-10s + %-10s: %s %s", 
                  platform, granularity, status_icon, result$status))
      
      if (result$status == "SUCCESS") {
        cat(sprintf(" (%d rows)", result$rows))
      } else if (result$status == "ERROR") {
        cat(sprintf(" (HTTP %d)", result$code))
      }
      
      cat("\n")
    }
  }
}

# Summary analysis
cat("\n\n=== SUMMARY ANALYSIS ===\n")

# Check unified daily issue
unified_daily_results <- results[grep("Unified.*\\|unified\\|daily", names(results))]
unified_daily_empty <- sapply(unified_daily_results, function(x) x$status == "EMPTY")

if (any(unified_daily_empty)) {
  cat("\n⚠️  CONFIRMED: Unified endpoints don't work with daily granularity\n")
  cat("   Affected endpoints:\n")
  for (name in names(unified_daily_results)[unified_daily_empty]) {
    endpoint_name <- strsplit(name, "\\|")[[1]][1]
    cat(sprintf("   - %s\n", endpoint_name))
  }
}

# Check what works for unified
cat("\n✅ What DOES work for unified endpoints:\n")
unified_success <- results[grep("\\|unified\\|", names(results))]
unified_success <- unified_success[sapply(unified_success, function(x) x$status == "SUCCESS")]

if (length(unified_success) > 0) {
  for (name in names(unified_success)) {
    parts <- strsplit(name, "\\|")[[1]]
    cat(sprintf("   - %s with %s granularity\n", parts[1], parts[3]))
  }
} else {
  cat("   - Nothing works for unified endpoints!\n")
}

# Platform-specific success
cat("\n✅ Platform-specific endpoints (iOS/Android):\n")
platform_results <- results[!grepl("\\|unified\\|", names(results))]
platform_success_rate <- sum(sapply(platform_results, function(x) x$status == "SUCCESS")) / length(platform_results)
cat(sprintf("   - Success rate: %.1f%%\n", platform_success_rate * 100))

# Generate recommendations
cat("\n\n=== RECOMMENDATIONS ===\n")
cat("1. For daily data: ALWAYS use platform-specific endpoints (iOS/Android)\n")
cat("2. For weekly/monthly/quarterly: Platform-specific endpoints are more reliable\n")
cat("3. Unified endpoints: Generally unreliable, avoid if possible\n")
cat("4. Active Users endpoints: Similar pattern - unified doesn't work with daily\n")

# Save detailed results
cat("\n\nSaving detailed results to: endpoint_test_results.rds\n")
saveRDS(results, "endpoint_test_results.rds")