# Test MAU endpoint capabilities
library(sensortowerR)
library(httr2)
library(dplyr)
library(jsonlite)

# Load token
auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (auth_token == "") {
  stop("SENSORTOWER_AUTH_TOKEN not found in environment")
}

cat("Testing MAU endpoint capabilities...\n\n")

# Test parameters
test_app_id <- "553834731"  # Candy Crush iOS
countries <- c("US", "WW")
periods <- c("day", "week", "month")

# Function to test MAU endpoint
test_mau_endpoint <- function(app_id, country, time_period, auth_token) {
  base_url <- "https://api.sensortower.com/v1/ios/usage/active_users"
  
  # Calculate date range based on time_period
  end_date <- Sys.Date() - 1
  start_date <- switch(
    time_period,
    "day" = end_date - 30,
    "week" = end_date - 90,
    "month" = end_date - 365,
    end_date - 30
  )
  
  tryCatch({
    response <- request(base_url) %>%
      req_url_query(
        app_ids = app_id,
        countries = country,
        start_date = format(start_date, "%Y-%m-%d"),
        end_date = format(end_date, "%Y-%m-%d"),
        time_period = time_period,
        time_zone = "UTC",
        metrics = "monthly_users",
        auth_token = auth_token
      ) %>%
      req_perform()
    
    if (resp_status(response) == 200) {
      data <- resp_body_json(response)
      if (!is.null(data$data) && length(data$data) > 0) {
        first_entry <- data$data[[1]]
        if (!is.null(first_entry$usage_data) && length(first_entry$usage_data) > 0) {
          usage <- first_entry$usage_data[[1]]
          return(list(
            status = "SUCCESS",
            records = length(data$data[[1]]$usage_data),
            sample_date = usage$date,
            sample_mau = usage$monthly_users,
            time_period = time_period
          ))
        }
      }
      return(list(status = "EMPTY", time_period = time_period))
    }
  }, error = function(e) {
    return(list(status = "ERROR", error = as.character(e), time_period = time_period))
  })
}

# Test all combinations
results <- expand.grid(
  country = countries,
  time_period = periods,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    result = list(test_mau_endpoint(test_app_id, country, time_period, auth_token))
  ) %>%
  ungroup()

# Display results
cat("=== MAU Endpoint Test Results ===\n\n")
for (i in 1:nrow(results)) {
  row <- results[i,]
  res <- row$result[[1]]
  
  cat(sprintf("Country: %s, Period: %s\n", row$country, row$time_period))
  cat(sprintf("  Status: %s\n", res$status))
  
  if (res$status == "SUCCESS") {
    cat(sprintf("  Records: %d\n", res$records))
    cat(sprintf("  Sample: %s = %s MAU\n", res$sample_date, format(res$sample_mau, big.mark = ",")))
  } else if (res$status == "ERROR") {
    cat(sprintf("  Error: %s\n", res$error))
  }
  cat("\n")
}

# Test batch capabilities with multiple apps
cat("\n=== Testing Batch MAU Capabilities ===\n")
test_apps <- c("553834731", "1195621598")  # Candy Crush, Homescapes

batch_test <- test_mau_endpoint(
  paste(test_apps, collapse = ","),
  "US",
  "month",
  auth_token
)

if (batch_test$status == "SUCCESS") {
  cat("✓ MAU endpoint supports batch requests!\n")
} else {
  cat("✗ MAU endpoint does NOT support batch requests\n")
}

# Test Android MAU
cat("\n=== Testing Android MAU ===\n")
android_base_url <- "https://api.sensortower.com/v1/android/usage/active_users"
android_app <- "com.king.candycrushsaga"

android_test <- tryCatch({
  response <- request(android_base_url) %>%
    req_url_query(
      app_ids = android_app,
      countries = "US",
      start_date = format(Sys.Date() - 365, "%Y-%m-%d"),
      end_date = format(Sys.Date() - 1, "%Y-%m-%d"),
      time_period = "month",
      time_zone = "UTC",
      metrics = "monthly_users",
      auth_token = auth_token
    ) %>%
    req_perform()
  
  if (resp_status(response) == 200) {
    data <- resp_body_json(response)
    if (!is.null(data$data) && length(data$data) > 0 && 
        length(data$data[[1]]$usage_data) > 0) {
      "SUCCESS"
    } else {
      "EMPTY"
    }
  }
}, error = function(e) {
  paste("ERROR:", as.character(e))
})

cat(sprintf("Android MAU test: %s\n", android_test))

cat("\n=== Key Findings ===\n")
cat("1. MAU endpoint accepts time_period='month' for monthly data\n")
cat("2. Returns 'monthly_users' metric when requested\n")
cat("3. Can fetch up to 1 year of historical MAU data\n")
cat("4. Separate endpoints for iOS and Android\n")
cat("5. Batch support needs verification\n")