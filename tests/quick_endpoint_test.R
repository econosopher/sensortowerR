# Quick Sensor Tower Endpoint Test
# Focuses on the key question: What works for daily data?

library(httr)
library(jsonlite)

# Setup
AUTH_TOKEN <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (AUTH_TOKEN == "") stop("Set SENSORTOWER_AUTH_TOKEN")

# Test app
HOMESCAPES <- list(
  ios = "1195621598",
  android = "com.playrix.homescapes",
  unified = "1195621598"
)

# Test date range (1 week)
dates <- list(
  start = as.character(Sys.Date() - 7),
  end = as.character(Sys.Date() - 1)
)

# Quick test function
quick_test <- function(url, params, desc) {
  cat(sprintf("\n%-50s", desc))
  
  resp <- GET(url, query = c(params, auth_token = AUTH_TOKEN))
  code <- status_code(resp)
  
  if (code == 200) {
    content <- content(resp, as = "text", encoding = "UTF-8")
    if (nchar(content) == 0 || content == "[]" || content == "{}") {
      cat("❌ Empty response\n")
      return(FALSE)
    }
    
    data <- tryCatch({
      fromJSON(content, flatten = TRUE)
    }, error = function(e) NULL)
    
    if (is.data.frame(data)) {
      cat(sprintf("✅ Success! %d rows\n", nrow(data)))
      return(TRUE)
    } else if (is.list(data)) {
      cat(sprintf("✅ Success! List with %d elements\n", length(data)))
      return(TRUE)
    } else {
      cat("❓ Unknown format\n")
      return(FALSE)
    }
  } else {
    cat(sprintf("❌ HTTP %d\n", code))
    return(FALSE)
  }
}

cat("=== SENSOR TOWER DAILY DATA ENDPOINT TEST ===\n")
cat(sprintf("Testing with Homescapes (%s)\n", dates$start))

# Test matrix
tests <- list(
  # Unified endpoints
  list(
    url = "https://api.sensortower.com/v1/unified/sales_report_estimates",
    params = list(
      app_ids = HOMESCAPES$unified,
      date_granularity = "daily",
      start_date = dates$start,
      end_date = dates$end
    ),
    desc = "1. Unified daily (no country)"
  ),
  list(
    url = "https://api.sensortower.com/v1/unified/sales_report_estimates",
    params = list(
      app_ids = HOMESCAPES$unified,
      countries = "US",
      date_granularity = "daily",
      start_date = dates$start,
      end_date = dates$end
    ),
    desc = "2. Unified daily (with US)"
  ),
  list(
    url = "https://api.sensortower.com/v1/unified/sales_report_estimates",
    params = list(
      app_ids = HOMESCAPES$unified,
      date_granularity = "monthly",
      start_date = dates$start,
      end_date = dates$end
    ),
    desc = "3. Unified monthly"
  ),
  
  # iOS endpoints
  list(
    url = "https://api.sensortower.com/v1/ios/sales_report_estimates",
    params = list(
      app_ids = HOMESCAPES$ios,
      countries = "US",
      date_granularity = "daily",
      start_date = dates$start,
      end_date = dates$end
    ),
    desc = "4. iOS daily"
  ),
  
  # Android endpoints
  list(
    url = "https://api.sensortower.com/v1/android/sales_report_estimates",
    params = list(
      app_ids = HOMESCAPES$android,
      countries = "US",
      date_granularity = "daily",
      start_date = dates$start,
      end_date = dates$end
    ),
    desc = "5. Android daily"
  ),
  
  # Unified usage
  list(
    url = "https://api.sensortower.com/v1/unified/usage/active_users",
    params = list(
      app_ids = HOMESCAPES$unified,
      time_period = "day",
      start_date = dates$start,
      end_date = dates$end
    ),
    desc = "6. Unified active users (daily)"
  )
)

# Run tests
results <- sapply(tests, function(test) {
  quick_test(test$url, test$params, test$desc)
})

# Summary
cat("\n=== SUMMARY ===\n")
cat(sprintf("Tests passed: %d/%d\n", sum(results), length(results)))

if (!results[1] && !results[2] && (results[4] || results[5])) {
  cat("\n📌 CONFIRMED: Unified endpoint doesn't work for daily data\n")
  cat("   Use platform-specific endpoints instead\n")
}

# Test with different app formats
cat("\n=== TESTING APP ID FORMATS ===\n")

# Test Android package name in unified
quick_test(
  "https://api.sensortower.com/v1/unified/sales_report_estimates",
  list(
    app_ids = HOMESCAPES$android,  # Android package name
    date_granularity = "daily",
    start_date = dates$start,
    end_date = dates$end
  ),
  "7. Unified with Android package name"
)

# Test multiple apps
quick_test(
  "https://api.sensortower.com/v1/ios/sales_report_estimates",
  list(
    app_ids = "1195621598,553834731",  # Homescapes + Candy Crush
    countries = "US",
    date_granularity = "daily",
    start_date = dates$start,
    end_date = dates$end
  ),
  "8. iOS multiple apps"
)

cat("\n✅ Test complete!\n")