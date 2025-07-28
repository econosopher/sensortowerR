# Comprehensive Sensor Tower API Endpoint Testing Script
# Tests various endpoint combinations to understand what works and what doesn't

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Configuration
AUTH_TOKEN <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (AUTH_TOKEN == "") {
  stop("Please set SENSORTOWER_AUTH_TOKEN environment variable")
}

# Test configuration
TEST_APP_IDS <- list(
  ios = c(
    "1195621598",  # Homescapes
    "553834731",   # Candy Crush Saga
    "1482155847"   # Royal Match
  ),
  android = c(
    "com.playrix.homescapes",
    "com.king.candycrushsaga",
    "com.dreamgames.royalmatch"
  ),
  unified = c(
    "1195621598",  # Numeric (iOS-like)
    "com.playrix.homescapes"  # Package (Android-like)
  )
)

# Date ranges to test
DATE_RANGES <- list(
  week = list(
    start = as.character(Sys.Date() - 7),
    end = as.character(Sys.Date() - 1)
  ),
  month = list(
    start = as.character(Sys.Date() - 30),
    end = as.character(Sys.Date() - 1)
  )
)

# Initialize results tracking
results <- data.frame()
log_message <- function(msg) {
  cat(sprintf("[%s] %s\n", Sys.time(), msg))
}

# Function to test an endpoint
test_endpoint <- function(endpoint, params, description) {
  log_message(sprintf("Testing: %s", description))
  
  start_time <- Sys.time()
  
  response <- tryCatch({
    GET(
      url = endpoint,
      query = c(params, auth_token = AUTH_TOKEN),
      timeout(30)
    )
  }, error = function(e) {
    return(list(error = e$message))
  })
  
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Process response
  if (!is.null(response$error)) {
    result <- list(
      endpoint = endpoint,
      description = description,
      status = "ERROR",
      http_code = NA,
      rows = 0,
      columns = NA,
      duration = duration,
      error = response$error,
      params = paste(names(params), params, sep = "=", collapse = ", ")
    )
  } else {
    http_code <- status_code(response)
    
    if (http_code == 200) {
      content_text <- content(response, as = "text", encoding = "UTF-8")
      
      if (nchar(content_text) == 0 || content_text == "[]" || content_text == "{}") {
        result <- list(
          endpoint = endpoint,
          description = description,
          status = "EMPTY",
          http_code = http_code,
          rows = 0,
          columns = NA,
          duration = duration,
          error = "Empty response",
          params = paste(names(params), params, sep = "=", collapse = ", ")
        )
      } else {
        parsed <- tryCatch({
          fromJSON(content_text, flatten = TRUE)
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.data.frame(parsed)) {
          result <- list(
            endpoint = endpoint,
            description = description,
            status = "SUCCESS",
            http_code = http_code,
            rows = nrow(parsed),
            columns = paste(names(parsed)[1:min(5, length(names(parsed)))], collapse = ", "),
            duration = duration,
            error = NA,
            params = paste(names(params), params, sep = "=", collapse = ", ")
          )
        } else if (is.list(parsed)) {
          result <- list(
            endpoint = endpoint,
            description = description,
            status = "SUCCESS_LIST",
            http_code = http_code,
            rows = length(parsed),
            columns = paste(names(parsed)[1:min(5, length(names(parsed)))], collapse = ", "),
            duration = duration,
            error = NA,
            params = paste(names(params), params, sep = "=", collapse = ", ")
          )
        } else {
          result <- list(
            endpoint = endpoint,
            description = description,
            status = "UNKNOWN_FORMAT",
            http_code = http_code,
            rows = NA,
            columns = NA,
            duration = duration,
            error = paste("Unknown format:", class(parsed)),
            params = paste(names(params), params, sep = "=", collapse = ", ")
          )
        }
      }
    } else {
      error_content <- tryCatch({
        content(response, as = "text", encoding = "UTF-8")
      }, error = function(e) {
        "Could not parse error message"
      })
      
      result <- list(
        endpoint = endpoint,
        description = description,
        status = "HTTP_ERROR",
        http_code = http_code,
        rows = 0,
        columns = NA,
        duration = duration,
        error = error_content,
        params = paste(names(params), params, sep = "=", collapse = ", ")
      )
    }
  }
  
  log_message(sprintf("  Status: %s, HTTP: %s, Rows: %s, Duration: %.1fs", 
                     result$status, 
                     ifelse(is.na(result$http_code), "NA", result$http_code),
                     ifelse(is.na(result$rows), "NA", result$rows),
                     result$duration))
  
  return(as.data.frame(result, stringsAsFactors = FALSE))
}

# Define test cases
log_message("Starting comprehensive endpoint tests")
log_message("=====================================")

# 1. Test unified sales_report_estimates
log_message("\n1. Testing unified sales_report_estimates")
for (app_id in TEST_APP_IDS$unified[1]) {  # Just test one
  for (date_range_name in names(DATE_RANGES)) {
    date_range <- DATE_RANGES[[date_range_name]]
    
    # Test daily
    result <- test_endpoint(
      "https://api.sensortower.com/v1/unified/sales_report_estimates",
      list(
        app_ids = app_id,
        date_granularity = "daily",
        start_date = date_range$start,
        end_date = date_range$end
      ),
      sprintf("Unified sales daily - %s - %s", app_id, date_range_name)
    )
    results <- bind_rows(results, result)
    
    # Test with countries
    result <- test_endpoint(
      "https://api.sensortower.com/v1/unified/sales_report_estimates",
      list(
        app_ids = app_id,
        countries = "US",
        date_granularity = "daily",
        start_date = date_range$start,
        end_date = date_range$end
      ),
      sprintf("Unified sales daily+country - %s - %s", app_id, date_range_name)
    )
    results <- bind_rows(results, result)
    
    # Test monthly
    result <- test_endpoint(
      "https://api.sensortower.com/v1/unified/sales_report_estimates",
      list(
        app_ids = app_id,
        date_granularity = "monthly",
        start_date = date_range$start,
        end_date = date_range$end
      ),
      sprintf("Unified sales monthly - %s - %s", app_id, date_range_name)
    )
    results <- bind_rows(results, result)
  }
}

# 2. Test iOS sales_report_estimates
log_message("\n2. Testing iOS sales_report_estimates")
for (app_id in TEST_APP_IDS$ios[1]) {  # Just test one
  result <- test_endpoint(
    "https://api.sensortower.com/v1/ios/sales_report_estimates",
    list(
      app_ids = app_id,
      countries = "US",
      date_granularity = "daily",
      start_date = DATE_RANGES$week$start,
      end_date = DATE_RANGES$week$end
    ),
    sprintf("iOS sales daily - %s", app_id)
  )
  results <- bind_rows(results, result)
}

# 3. Test Android sales_report_estimates
log_message("\n3. Testing Android sales_report_estimates")
for (app_id in TEST_APP_IDS$android[1]) {  # Just test one
  result <- test_endpoint(
    "https://api.sensortower.com/v1/android/sales_report_estimates",
    list(
      app_ids = app_id,
      countries = "US",
      date_granularity = "daily",
      start_date = DATE_RANGES$week$start,
      end_date = DATE_RANGES$week$end
    ),
    sprintf("Android sales daily - %s", app_id)
  )
  results <- bind_rows(results, result)
}

# 4. Test unified usage/active_users
log_message("\n4. Testing unified usage/active_users")
for (app_id in TEST_APP_IDS$unified[1]) {
  result <- test_endpoint(
    "https://api.sensortower.com/v1/unified/usage/active_users",
    list(
      app_ids = app_id,
      time_period = "day",
      start_date = DATE_RANGES$week$start,
      end_date = DATE_RANGES$week$end
    ),
    sprintf("Unified active users - %s", app_id)
  )
  results <- bind_rows(results, result)
}

# 5. Test compact endpoints
log_message("\n5. Testing compact sales endpoints")
result <- test_endpoint(
  "https://api.sensortower.com/v1/ios/compact_sales_report_estimates",
  list(
    app_ids = paste(TEST_APP_IDS$ios[1:2], collapse = ","),
    countries = "US",
    date_granularity = "daily",
    start_date = DATE_RANGES$week$start,
    end_date = DATE_RANGES$week$end
  ),
  "iOS compact sales - multiple apps"
)
results <- bind_rows(results, result)

# 6. Test different parameter combinations
log_message("\n6. Testing parameter combinations")
test_params <- list(
  list(
    endpoint = "https://api.sensortower.com/v1/unified/sales_report_estimates",
    params = list(app_ids = TEST_APP_IDS$unified[1]),
    desc = "Minimal params - just app_id"
  ),
  list(
    endpoint = "https://api.sensortower.com/v1/unified/sales_report_estimates",
    params = list(
      app_ids = TEST_APP_IDS$unified[1],
      date_granularity = "weekly"
    ),
    desc = "Weekly granularity"
  ),
  list(
    endpoint = "https://api.sensortower.com/v1/unified/sales_report_estimates",
    params = list(
      app_ids = TEST_APP_IDS$unified[1],
      date_granularity = "quarterly"
    ),
    desc = "Quarterly granularity"
  )
)

for (test in test_params) {
  result <- test_endpoint(test$endpoint, test$params, test$desc)
  results <- bind_rows(results, result)
}

# Save results
write.csv(results, "endpoint_test_results.csv", row.names = FALSE)

# Create summary report
log_message("\n=====================================")
log_message("SUMMARY REPORT")
log_message("=====================================")

# Group by endpoint and status
summary_report <- results %>%
  group_by(endpoint, status) %>%
  summarise(
    count = n(),
    avg_duration = mean(duration, na.rm = TRUE),
    avg_rows = mean(rows, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = status,
    values_from = count,
    values_fill = 0
  )

print(summary_report)

# Specific findings
log_message("\n\nKEY FINDINGS:")

# Check unified daily
unified_daily <- results %>%
  filter(
    grepl("unified.*sales", endpoint),
    grepl("daily", params)
  )

if (all(unified_daily$status %in% c("EMPTY", "ERROR"))) {
  log_message("❌ Unified sales_report_estimates does NOT work with daily granularity")
} else {
  log_message("✅ Unified sales_report_estimates DOES work with daily granularity")
}

# Check platform-specific daily
platform_daily <- results %>%
  filter(
    grepl("(ios|android)/sales", endpoint),
    grepl("daily", params)
  )

if (any(platform_daily$status == "SUCCESS")) {
  log_message("✅ Platform-specific sales_report_estimates DOES work with daily granularity")
}

# Successful parameter combinations
successful <- results %>%
  filter(status == "SUCCESS") %>%
  select(description, rows, duration)

log_message("\nSuccessful endpoint combinations:")
print(successful)

# Create visual summary
library(ggplot2)
endpoint_plot <- ggplot(results, aes(x = endpoint, fill = status)) +
  geom_bar(position = "stack") +
  coord_flip() +
  labs(
    title = "Sensor Tower API Endpoint Test Results",
    x = "Endpoint",
    y = "Number of Tests",
    fill = "Status"
  ) +
  scale_fill_manual(values = c(
    "SUCCESS" = "#2ECC71",
    "EMPTY" = "#F39C12", 
    "ERROR" = "#E74C3C",
    "HTTP_ERROR" = "#C0392B",
    "SUCCESS_LIST" = "#27AE60",
    "UNKNOWN_FORMAT" = "#95A5A6"
  )) +
  theme_minimal()

ggsave("endpoint_test_results.png", endpoint_plot, width = 12, height = 8, dpi = 150)

log_message("\nTest complete. Results saved to:")
log_message("  - endpoint_test_results.csv")
log_message("  - endpoint_test_results.png")