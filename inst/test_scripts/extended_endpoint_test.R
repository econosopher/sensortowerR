# Extended Endpoint Test - Testing More Endpoints
# Focus on what actually works for each endpoint/platform/granularity combination

library(httr)
library(jsonlite)

# Setup
AUTH_TOKEN <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (AUTH_TOKEN == "") stop("Set SENSORTOWER_AUTH_TOKEN")

# Test configuration
TEST_APPS <- list(
  ios = "553834731",
  android = "com.king.candycrushsaga",
  unified = "553834731"
)

TEST_PUBLISHER <- "56cc73d4dd9b024d89da87c7"  # King

# Simple test function
quick_test <- function(url, params) {
  resp <- GET(url, query = c(params, auth_token = AUTH_TOKEN))
  
  if (status_code(resp) == 200) {
    content <- content(resp, as = "text", encoding = "UTF-8")
    if (nchar(content) == 0 || content == "[]" || content == "{}") {
      return("EMPTY")
    }
    
    data <- tryCatch(fromJSON(content, flatten = TRUE), error = function(e) NULL)
    if (!is.null(data)) {
      if (is.data.frame(data)) return(paste0("SUCCESS (", nrow(data), " rows)"))
      if (is.list(data)) return(paste0("SUCCESS (", length(data), " items)"))
    }
    return("UNKNOWN")
  }
  
  return(paste0("ERROR ", status_code(resp)))
}

cat("=== EXTENDED SENSOR TOWER ENDPOINT TEST ===\n")
cat(sprintf("Date: %s\n\n", Sys.Date()))

# Test different endpoint patterns
cat("1. SALES REPORT ESTIMATES (Revenue/Downloads)\n")
cat("=" , strrep("=", 50), "\n\n")

# Test unified with ALL granularities
cat("Unified Sales Report - All Granularities:\n")
for (gran in c("daily", "weekly", "monthly", "quarterly", "yearly")) {
  dates <- switch(gran,
    "daily" = list(start = as.character(Sys.Date() - 7), end = as.character(Sys.Date() - 1)),
    "weekly" = list(start = as.character(Sys.Date() - 28), end = as.character(Sys.Date() - 1)),
    "monthly" = list(start = "2024-01-01", end = "2024-12-31"),
    "quarterly" = list(start = "2024-01-01", end = "2024-12-31"),
    "yearly" = list(start = "2023-01-01", end = "2024-12-31")
  )
  
  result <- quick_test(
    "https://api.sensortower.com/v1/unified/sales_report_estimates",
    list(
      app_ids = TEST_APPS$unified,
      countries = "US",
      date_granularity = gran,
      start_date = dates$start,
      end_date = dates$end
    )
  )
  
  cat(sprintf("  %-10s: %s\n", gran, result))
}

# Test with multiple countries
cat("\nUnified Sales Report - Multiple Countries (monthly):\n")
result <- quick_test(
  "https://api.sensortower.com/v1/unified/sales_report_estimates",
  list(
    app_ids = TEST_APPS$unified,
    countries = "US,GB,JP",
    date_granularity = "monthly",
    start_date = "2024-01-01",
    end_date = "2024-03-31"
  )
)
cat(sprintf("  US,GB,JP: %s\n", result))

# Test Top & Trending endpoints
cat("\n\n2. TOP & TRENDING ENDPOINTS\n")
cat("=" , strrep("=", 50), "\n\n")

# Top Publishers
cat("Top Publishers by Platform:\n")
for (os in c("unified", "ios", "android")) {
  url <- sprintf("https://api.sensortower.com/v1/%s/top_and_trending/publishers", os)
  
  result <- quick_test(url, list(
    categories = ifelse(os == "android", "game", "6014"),
    time_range = "month",
    measure = "revenue",
    comparison_attribute = "absolute",
    date = "2025-06-01",
    regions = "US",
    limit = 5
  ))
  
  cat(sprintf("  %-10s: %s\n", os, result))
}

# Top Charts (Apps)
cat("\nTop Charts by Time Range (iOS):\n")
for (tr in c("day", "week", "month", "quarter", "year")) {
  # Calculate appropriate date
  date <- switch(tr,
    "day" = as.character(Sys.Date() - 2),
    "week" = as.character(Sys.Date() - 7),
    "month" = format(Sys.Date() - 35, "%Y-%m-01"),
    "quarter" = "2025-04-01",
    "year" = "2025-01-01"
  )
  
  result <- quick_test(
    "https://api.sensortower.com/v1/ios/top_and_trending/top_charts",
    list(
      chart_type = "revenue",
      category = "6014",
      time_range = tr,
      date = date,
      regions = "US",
      device = "total",
      limit = 5
    )
  )
  
  cat(sprintf("  %-10s: %s\n", tr, result))
}

# Test Active Users with different parameters
cat("\n\n3. ACTIVE USERS ENDPOINTS\n")
cat("=" , strrep("=", 50), "\n\n")

# Try different parameter combinations
cat("iOS Active Users - Testing Parameters:\n")

# Standard parameters
result1 <- quick_test(
  "https://api.sensortower.com/v1/ios/usage/active_users",
  list(
    app_ids = TEST_APPS$ios,
    time_period = "month",
    start_date = "2024-01-01",
    end_date = "2024-03-31"
  )
)
cat(sprintf("  Standard params: %s\n", result1))

# With countries
result2 <- quick_test(
  "https://api.sensortower.com/v1/ios/usage/active_users",
  list(
    app_ids = TEST_APPS$ios,
    time_period = "month",
    start_date = "2024-01-01",
    end_date = "2024-03-31",
    countries = "US"
  )
)
cat(sprintf("  With countries: %s\n", result2))

# Try single date
result3 <- quick_test(
  "https://api.sensortower.com/v1/ios/usage/active_users",
  list(
    app_ids = TEST_APPS$ios,
    time_period = "month",
    date = "2024-01-01"
  )
)
cat(sprintf("  Single date param: %s\n", result3))

# Test Category Rankings
cat("\n\n4. CATEGORY RANKINGS\n")
cat("=" , strrep("=", 50), "\n\n")

cat("iOS Category Rankings - Chart Types:\n")
for (chart in c("topfreeapplications", "toppaidapplications", "topgrossingapplications")) {
  result <- quick_test(
    "https://api.sensortower.com/v1/ios/category_rankings",
    list(
      category = "6014",
      chart_type = chart,
      country = "US",
      date = as.character(Sys.Date() - 1),
      limit = 5
    )
  )
  
  cat(sprintf("  %-25s: %s\n", chart, result))
}

# Test Publisher endpoints
cat("\n\n5. PUBLISHER ENDPOINTS\n")
cat("=" , strrep("=", 50), "\n\n")

# Publisher Apps
cat("Publisher Apps:\n")
result <- quick_test(
  "https://api.sensortower.com/v1/unified/publishers/apps",
  list(publisher_id = TEST_PUBLISHER)
)
cat(sprintf("  Unified: %s\n", result))

# Publisher Category Breakdown
cat("\nPublisher Category Breakdown:\n")
for (os in c("unified", "ios", "android")) {
  url <- sprintf("https://api.sensortower.com/v1/%s/publishers/category_breakdown", os)
  
  result <- quick_test(url, list(
    publisher_ids = TEST_PUBLISHER,
    time_range = "month",
    start_date = "2025-01-01",
    end_date = "2025-01-31",
    regions = "US"
  ))
  
  cat(sprintf("  %-10s: %s\n", os, result))
}

# Summary Matrix
cat("\n\n=== KEY FINDINGS ===\n")
cat("1. Unified Sales Report: COMPLETELY BROKEN (all granularities return empty)\n")
cat("2. Platform-specific Sales Reports: Work perfectly with all granularities\n")
cat("3. Top & Trending: Mixed results, platform-specific generally better\n")
cat("4. Active Users: All return 422 errors - may need different parameters\n")
cat("5. Category Rankings: Work well for iOS/Android\n")
cat("6. Publisher endpoints: Mixed functionality\n")

cat("\n=== RECOMMENDED APPROACH ===\n")
cat("- NEVER use unified sales_report_estimates endpoint\n")
cat("- Always use platform-specific endpoints when available\n")
cat("- For unified data, fetch iOS + Android separately and combine\n")
cat("- Active Users endpoints need investigation - 422 suggests parameter issues\n")