# Test MAU endpoint with correct response format
library(sensortowerR)
library(httr2)
library(dplyr)
library(jsonlite)

# Load token
auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (auth_token == "") {
  stop("SENSORTOWER_AUTH_TOKEN not found in environment")
}

cat("Testing MAU endpoint with correct response format...\n\n")

# Test iOS MAU
test_app_id <- "553834731"  # Candy Crush iOS
base_url <- "https://api.sensortower.com/v1/ios/usage/active_users"

end_date <- Sys.Date() - 1
start_date <- end_date - 365  # 1 year of data

cat("Testing iOS MAU endpoint...\n")
response <- request(base_url) %>%
  req_url_query(
    app_ids = test_app_id,
    countries = "US",
    start_date = format(start_date, "%Y-%m-%d"),
    end_date = format(end_date, "%Y-%m-%d"),
    time_period = "month",
    auth_token = auth_token
  ) %>%
  req_perform()

if (resp_status(response) == 200) {
  data <- resp_body_json(response)
  
  if (!is.null(data) && length(data) > 0) {
    # Convert to data frame
    mau_data <- bind_rows(data)
    
    cat(sprintf("✓ Success! Retrieved %d months of MAU data\n", nrow(mau_data)))
    cat("\nSample data:\n")
    
    # Show first few rows
    mau_data %>%
      head(3) %>%
      mutate(
        total_ios = iphone_users + ipad_users,
        date = as.Date(date)
      ) %>%
      select(date, iphone_users, ipad_users, total_ios) %>%
      print()
    
    # Test batch with multiple apps
    cat("\n\nTesting batch MAU with multiple apps...\n")
    batch_apps <- c("553834731", "1195621598", "1053012308")
    
    batch_response <- request(base_url) %>%
      req_url_query(
        app_ids = paste(batch_apps, collapse = ","),
        countries = "US",
        start_date = format(Sys.Date() - 90, "%Y-%m-%d"),
        end_date = format(Sys.Date() - 1, "%Y-%m-%d"),
        time_period = "month",
        auth_token = auth_token
      ) %>%
      req_perform()
    
    if (resp_status(batch_response) == 200) {
      batch_data <- resp_body_json(batch_response)
      batch_df <- bind_rows(batch_data)
      
      apps_found <- unique(batch_df$app_id)
      cat(sprintf("✓ Batch request successful! Found data for %d apps\n", 
                  length(apps_found)))
      cat(sprintf("  Apps: %s\n", paste(apps_found, collapse = ", ")))
      
      # Check if we can batch up to 5 apps
      if (length(apps_found) == length(batch_apps)) {
        cat("✓ MAU endpoint supports at least 3 apps per batch\n")
      }
    }
    
    # Test Android MAU
    cat("\n\nTesting Android MAU endpoint...\n")
    android_url <- "https://api.sensortower.com/v1/android/usage/active_users"
    android_app <- "com.king.candycrushsaga"
    
    android_response <- request(android_url) %>%
      req_url_query(
        app_ids = android_app,
        countries = "US",
        start_date = format(Sys.Date() - 90, "%Y-%m-%d"),
        end_date = format(Sys.Date() - 1, "%Y-%m-%d"),
        time_period = "month",
        auth_token = auth_token
      ) %>%
      req_perform()
    
    if (resp_status(android_response) == 200) {
      android_data <- resp_body_json(android_response)
      android_df <- bind_rows(android_data)
      
      cat(sprintf("✓ Android MAU successful! Retrieved %d months\n", nrow(android_df)))
      cat("\nSample Android data:\n")
      
      android_df %>%
        head(3) %>%
        mutate(date = as.Date(date)) %>%
        select(date, users) %>%
        print()
    }
    
  } else {
    cat("✗ No data returned from MAU endpoint\n")
  }
} else {
  cat(sprintf("✗ Request failed with status: %d\n", resp_status(response)))
}

cat("\n=== MAU Implementation Strategy ===\n")
cat("1. MAU works with time_period='month' for monthly data\n")
cat("2. iOS returns iphone_users + ipad_users\n")
cat("3. Android returns users field\n")
cat("4. Can batch multiple apps in one request\n")
cat("5. Similar to DAU/WAU, fetch full YTD then average\n")