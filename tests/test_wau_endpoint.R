# Test WAU (Weekly Active Users) Endpoint
library(httr)
library(jsonlite)

# Setup
AUTH_TOKEN <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (AUTH_TOKEN == "") stop("Set SENSORTOWER_AUTH_TOKEN")

cat("=== TESTING WAU ENDPOINT ===\n")
cat(sprintf("Date: %s\n\n", Sys.Date()))

# Test 1: Basic WAU request
cat("1. Testing basic WAU request\n")
cat("=" , strrep("=", 50), "\n")

response <- GET(
  "https://api.sensortower.com/v1/ios/usage/active_users",
  query = list(
    app_ids = "553834731",  # Candy Crush
    time_period = "week",   # Weekly granularity
    start_date = "2025-06-01",
    end_date = "2025-07-21",
    countries = "US",
    auth_token = AUTH_TOKEN
  )
)

if (status_code(response) == 200) {
  data <- fromJSON(content(response, as = "text"), flatten = TRUE)
  
  cat(sprintf("  Status: SUCCESS\n"))
  cat(sprintf("  Rows returned: %d\n", nrow(data)))
  cat(sprintf("  Columns: %s\n", paste(names(data), collapse = ", ")))
  
  # Show sample data
  if (nrow(data) > 0) {
    cat("\n  Sample data:\n")
    us_data <- data[data$country == "US", ]
    
    for (i in 1:min(5, nrow(us_data))) {
      total_users <- us_data$iphone_users[i] + us_data$ipad_users[i]
      cat(sprintf("    Week ending %s: %s users\n", 
                  substr(us_data$date[i], 1, 10),
                  format(total_users, big.mark = ",")))
    }
    
    # Calculate average WAU
    us_data$total_users <- us_data$iphone_users + us_data$ipad_users
    n_weeks <- nrow(us_data)
    avg_wau <- mean(us_data$total_users)
    
    cat(sprintf("\n  Total weeks: %d\n", n_weeks))
    cat(sprintf("  Average WAU: %s\n", format(round(avg_wau), big.mark = ",")))
  }
} else {
  cat(sprintf("  Status: ERROR %d\n", status_code(response)))
}

# Test 2: Multiple apps WAU
cat("\n\n2. Testing multiple apps WAU\n")
cat("=" , strrep("=", 50), "\n")

response2 <- GET(
  "https://api.sensortower.com/v1/ios/usage/active_users",
  query = list(
    app_ids = "553834731,1195621598",  # Candy Crush, Homescapes
    time_period = "week",
    start_date = "2025-07-01",
    end_date = "2025-07-21",
    countries = "US",
    auth_token = AUTH_TOKEN
  )
)

if (status_code(response2) == 200) {
  data2 <- fromJSON(content(response2, as = "text"), flatten = TRUE)
  
  cat(sprintf("  Status: SUCCESS\n"))
  cat(sprintf("  Rows returned: %d\n", nrow(data2)))
  
  # Count unique apps
  n_apps <- length(unique(data2$app_id))
  cat(sprintf("  Unique apps: %d\n", n_apps))
  
  # Show data per app
  for (app in unique(data2$app_id)) {
    app_data <- data2[data2$app_id == app & data2$country == "US", ]
    app_data$total_users <- app_data$iphone_users + app_data$ipad_users
    avg_wau <- mean(app_data$total_users)
    cat(sprintf("  App %s: %d weeks, avg WAU = %s\n", 
                app, nrow(app_data), format(round(avg_wau), big.mark = ",")))
  }
}

# Test 3: Android WAU
cat("\n\n3. Testing Android WAU\n")
cat("=" , strrep("=", 50), "\n")

response3 <- GET(
  "https://api.sensortower.com/v1/android/usage/active_users",
  query = list(
    app_ids = "com.king.candycrushsaga",
    time_period = "week",
    start_date = "2025-07-01",
    end_date = "2025-07-21",
    countries = "US",
    auth_token = AUTH_TOKEN
  )
)

if (status_code(response3) == 200) {
  data3 <- fromJSON(content(response3, as = "text"), flatten = TRUE)
  
  cat(sprintf("  Status: SUCCESS\n"))
  cat(sprintf("  Rows returned: %d\n", nrow(data3)))
  
  if (nrow(data3) > 0) {
    us_data3 <- data3[data3$country == "US", ]
    cat(sprintf("  Android WAU field: %s\n", 
                if("users" %in% names(data3)) "users" else "unknown"))
    
    # Show sample
    for (i in 1:min(3, nrow(us_data3))) {
      cat(sprintf("    Week ending %s: %s users\n", 
                  substr(us_data3$date[i], 1, 10),
                  format(us_data3$users[i], big.mark = ",")))
    }
  }
}

# Test 4: Date alignment
cat("\n\n4. Testing date alignment for weeks\n")
cat("=" , strrep("=", 50), "\n")

# Test different start dates to see how weeks are aligned
test_dates <- list(
  "Sunday" = "2025-07-06",    # Sunday
  "Monday" = "2025-07-07",    # Monday
  "Wednesday" = "2025-07-09", # Wednesday
  "Saturday" = "2025-07-12"   # Saturday
)

for (day_name in names(test_dates)) {
  start_date <- test_dates[[day_name]]
  
  resp <- GET(
    "https://api.sensortower.com/v1/ios/usage/active_users",
    query = list(
      app_ids = "553834731",
      time_period = "week",
      start_date = start_date,
      end_date = as.character(as.Date(start_date) + 14), # 2 weeks later
      countries = "US", 
      auth_token = AUTH_TOKEN
    )
  )
  
  if (status_code(resp) == 200) {
    data <- fromJSON(content(resp, as = "text"), flatten = TRUE)
    us_data <- data[data$country == "US", ]
    
    if (nrow(us_data) > 0) {
      cat(sprintf("\n  Start date %s (%s):\n", start_date, day_name))
      cat(sprintf("    First week ends: %s\n", substr(us_data$date[1], 1, 10)))
      cat(sprintf("    Total weeks returned: %d\n", nrow(us_data)))
    }
  }
}

cat("\n\n=== WAU IMPLEMENTATION NOTES ===\n")
cat("1. WAU endpoint uses same URL as DAU but with time_period='week'\n")
cat("2. iOS returns iphone_users + ipad_users\n")
cat("3. Android returns single 'users' field\n")
cat("4. Weeks appear to end on Sundays\n")
cat("5. Can batch multiple apps in one request\n")
cat("6. Average WAU = Sum of weekly users / Number of weeks\n")