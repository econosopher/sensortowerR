# Test Active User Metrics Limitations
# This script tests the limitations of active user metrics in batch and unified requests

library(sensortowerR)
library(httr2)
library(dplyr)

cat("========================================\n")
cat("Active User Metrics Limitations Test\n")
cat("========================================\n\n")

auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (auth_token == "") {
  stop("SENSORTOWER_AUTH_TOKEN not found in environment")
}

# Test apps
test_apps <- list(
  ios = "553834731",  # Candy Crush iOS
  android = "com.king.candycrushsaga",  # Candy Crush Android
  unified = "55c5028802ac64f9c0001faf"  # Candy Crush Unified
)

# Test date range
end_date <- "2024-01-31"
start_date <- "2024-01-01"

# Test 1: Can we batch multiple apps in a single active users request?
cat("Test 1: Testing batch requests for active users\n")
cat("------------------------------------------------\n")

# Try iOS batch request
cat("\nTrying iOS batch request with 2 apps...\n")
ios_batch_url <- "https://api.sensortower.com/v1/ios/usage/active_users"

test_ios_apps <- c("553834731", "1195621598")  # Candy Crush and Match 3D
batch_request <- request(ios_batch_url) %>%
  req_url_query(
    app_ids = paste(test_ios_apps, collapse = ","),
    countries = "US",
    start_date = start_date,
    end_date = end_date,
    time_period = "month",
    auth_token = auth_token
  )

tryCatch({
  response <- req_perform(batch_request)
  if (resp_status(response) == 200) {
    data <- resp_body_json(response)
    if (!is.null(data) && length(data) > 0) {
      cat("✓ SUCCESS: Batch request accepted!\n")
      cat(sprintf("  Retrieved data for %d records\n", length(data)))
      
      # Check if we got data for both apps
      df <- bind_rows(data)
      unique_apps <- unique(df$app_id)
      cat(sprintf("  Unique apps in response: %d\n", length(unique_apps)))
      cat("  App IDs:", paste(unique_apps, collapse = ", "), "\n")
    }
  }
}, error = function(e) {
  cat("✗ FAILED: Batch request failed\n")
  cat("  Error:", e$message, "\n")
})

# Test 2: Can we use unified IDs for active users?
cat("\n\nTest 2: Testing unified ID support\n")
cat("------------------------------------------------\n")

unified_url <- "https://api.sensortower.com/v1/unified/usage/active_users"
cat("\nTrying unified endpoint with unified app ID...\n")

unified_request <- request(unified_url) %>%
  req_url_query(
    app_ids = test_apps$unified,
    countries = "US",
    start_date = start_date,
    end_date = end_date,
    time_period = "month",
    auth_token = auth_token
  )

tryCatch({
  response <- req_perform(unified_request)
  if (resp_status(response) == 200) {
    data <- resp_body_json(response)
    if (!is.null(data) && length(data) > 0) {
      cat("✓ SUCCESS: Unified ID accepted!\n")
      cat(sprintf("  Retrieved %d records\n", length(data)))
      
      # Check data structure
      df <- bind_rows(data)
      cat("  Columns:", paste(names(df), collapse = ", "), "\n")
    }
  }
}, error = function(e) {
  cat("✗ FAILED: Unified ID request failed\n")
  cat("  Error:", e$message, "\n")
})

# Test 3: Count API calls for multiple apps
cat("\n\nTest 3: Counting API calls for multiple apps\n")
cat("------------------------------------------------\n")

test_multiple_apps <- c("553834731", "1195621598", "529479190")  # 3 popular games
call_count <- 0

cat(sprintf("\nFetching active users for %d apps individually...\n", length(test_multiple_apps)))

for (app_id in test_multiple_apps) {
  cat(sprintf("  Fetching app %s... ", app_id))
  
  single_request <- request(ios_batch_url) %>%
    req_url_query(
      app_ids = app_id,
      countries = "US",
      start_date = start_date,
      end_date = end_date,
      time_period = "month",
      auth_token = auth_token
    )
  
  tryCatch({
    response <- req_perform(single_request)
    call_count <- call_count + 1
    
    if (resp_status(response) == 200) {
      data <- resp_body_json(response)
      cat(sprintf("✓ (%d records)\n", length(data)))
    }
  }, error = function(e) {
    cat("✗ Failed\n")
  })
  
  # Small delay to avoid rate limits
  Sys.sleep(0.5)
}

cat(sprintf("\nTotal API calls made: %d\n", call_count))
cat(sprintf("Calls per app: %.1f\n", call_count / length(test_multiple_apps)))

# Test 4: Check if st_batch_metrics handles active users
cat("\n\nTest 4: Testing st_batch_metrics with active users\n")
cat("------------------------------------------------\n")

cat("\nAttempting to fetch DAU/WAU/MAU via st_batch_metrics...\n")

batch_result <- tryCatch({
  st_batch_metrics(
    os = "ios",
    app_list = test_multiple_apps,
    metrics = c("dau", "wau", "mau"),
    date_range = list(start_date = start_date, end_date = end_date),
    countries = "US",
    granularity = "monthly",
    verbose = TRUE
  )
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
  NULL
})

if (!is.null(batch_result) && nrow(batch_result) > 0) {
  cat("\n✓ st_batch_metrics returned data!\n")
  cat("  Rows:", nrow(batch_result), "\n")
  cat("  Metrics:", paste(unique(batch_result$metric), collapse = ", "), "\n")
} else {
  cat("\n✗ st_batch_metrics did not return active user data\n")
}

# Summary
cat("\n\n========================================\n")
cat("SUMMARY OF FINDINGS\n")
cat("========================================\n")

cat("\n1. Batch Requests: Can the API accept multiple app IDs in one request?\n")
cat("2. Unified IDs: Can unified IDs be used for active user metrics?\n")
cat("3. API Call Efficiency: How many calls are needed for multiple apps?\n")
cat("4. Package Support: Does st_batch_metrics handle active users properly?\n")

cat("\nMitigation strategies to investigate:\n")
cat("- Implement rate limiting to prevent API overload\n")
cat("- Add console warnings when fetching active users for many apps\n")
cat("- Consider caching active user data more aggressively\n")
cat("- Document limitations clearly in function help\n")