# Parallel Endpoint Testing for Maximum Efficiency
# Uses parallel processing to test multiple endpoints simultaneously

library(httr)
library(jsonlite)
library(parallel)
library(data.table)

# Setup
AUTH_TOKEN <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (AUTH_TOKEN == "") stop("Set SENSORTOWER_AUTH_TOKEN")

# Define all test cases upfront
test_cases <- list(
  # Unified endpoints - various combinations
  list(id = "U1", endpoint = "unified/sales_report_estimates", 
       params = list(app_ids = "1195621598", date_granularity = "daily")),
  
  list(id = "U2", endpoint = "unified/sales_report_estimates", 
       params = list(app_ids = "1195621598", countries = "US", date_granularity = "daily")),
  
  list(id = "U3", endpoint = "unified/sales_report_estimates", 
       params = list(app_ids = "1195621598", date_granularity = "weekly")),
  
  list(id = "U4", endpoint = "unified/sales_report_estimates", 
       params = list(app_ids = "1195621598", date_granularity = "monthly")),
  
  list(id = "U5", endpoint = "unified/sales_report_estimates", 
       params = list(app_ids = "com.playrix.homescapes", date_granularity = "daily")),
  
  # Platform-specific endpoints
  list(id = "I1", endpoint = "ios/sales_report_estimates", 
       params = list(app_ids = "1195621598", countries = "US", date_granularity = "daily")),
  
  list(id = "A1", endpoint = "android/sales_report_estimates", 
       params = list(app_ids = "com.playrix.homescapes", countries = "US", date_granularity = "daily")),
  
  # Usage endpoints
  list(id = "UU1", endpoint = "unified/usage/active_users", 
       params = list(app_ids = "1195621598", time_period = "day")),
  
  # Compact endpoints
  list(id = "IC1", endpoint = "ios/compact_sales_report_estimates", 
       params = list(app_ids = "1195621598", countries = "US", date_granularity = "daily")),
  
  # Multiple apps
  list(id = "I2", endpoint = "ios/sales_report_estimates", 
       params = list(app_ids = "1195621598,553834731", countries = "US", date_granularity = "daily"))
)

# Add date ranges to all
dates <- list(
  start_date = as.character(Sys.Date() - 7),
  end_date = as.character(Sys.Date() - 1)
)

for (i in seq_along(test_cases)) {
  if (!any(c("start_date", "end_date") %in% names(test_cases[[i]]$params))) {
    test_cases[[i]]$params <- c(test_cases[[i]]$params, dates)
  }
}

# Test function for parallel execution
test_endpoint <- function(test_case) {
  url <- paste0("https://api.sensortower.com/v1/", test_case$endpoint)
  params <- c(test_case$params, auth_token = AUTH_TOKEN)
  
  start_time <- Sys.time()
  
  result <- tryCatch({
    resp <- GET(url, query = params, timeout(10))
    code <- status_code(resp)
    
    if (code == 200) {
      content <- content(resp, as = "text", encoding = "UTF-8")
      
      if (nchar(content) == 0 || content == "[]" || content == "{}") {
        list(id = test_case$id, status = "EMPTY", rows = 0, time = difftime(Sys.time(), start_time, units = "secs"))
      } else {
        data <- tryCatch({
          fromJSON(content, flatten = TRUE)
        }, error = function(e) NULL)
        
        if (is.data.frame(data)) {
          list(id = test_case$id, status = "SUCCESS", rows = nrow(data), time = difftime(Sys.time(), start_time, units = "secs"))
        } else if (is.list(data)) {
          list(id = test_case$id, status = "SUCCESS_LIST", rows = length(data), time = difftime(Sys.time(), start_time, units = "secs"))
        } else {
          list(id = test_case$id, status = "PARSE_ERROR", rows = NA, time = difftime(Sys.time(), start_time, units = "secs"))
        }
      }
    } else {
      list(id = test_case$id, status = paste0("HTTP_", code), rows = NA, time = difftime(Sys.time(), start_time, units = "secs"))
    }
  }, error = function(e) {
    list(id = test_case$id, status = "ERROR", rows = NA, time = difftime(Sys.time(), start_time, units = "secs"), error = e$message)
  })
  
  # Add endpoint info
  result$endpoint <- test_case$endpoint
  result$params <- paste(names(test_case$params), test_case$params, sep = "=", collapse = ", ")
  
  return(result)
}

cat("=== PARALLEL SENSOR TOWER ENDPOINT TESTING ===\n")
cat(sprintf("Testing %d endpoints in parallel...\n\n", length(test_cases)))

# Detect cores (use less than max to be nice)
n_cores <- min(detectCores() - 1, 4, length(test_cases))
cat(sprintf("Using %d parallel workers\n\n", n_cores))

# Run tests in parallel
start_time <- Sys.time()

if (.Platform$OS.type == "windows") {
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, {
    library(httr)
    library(jsonlite)
  })
  clusterExport(cl, c("AUTH_TOKEN"))
  results <- parLapply(cl, test_cases, test_endpoint)
  stopCluster(cl)
} else {
  results <- mclapply(test_cases, test_endpoint, mc.cores = n_cores)
}

total_time <- difftime(Sys.time(), start_time, units = "secs")

# Convert to data frame
results_df <- rbindlist(results, fill = TRUE)

# Print results table
cat("\nRESULTS TABLE:\n")
cat(sprintf("%-4s %-40s %-15s %6s %6s\n", "ID", "Endpoint", "Status", "Rows", "Time"))
cat(rep("-", 75), "\n", sep = "")

for (i in 1:nrow(results_df)) {
  cat(sprintf("%-4s %-40s %-15s %6s %5.1fs\n", 
              results_df$id[i],
              substr(results_df$endpoint[i], 1, 40),
              results_df$status[i],
              ifelse(is.na(results_df$rows[i]), "-", as.character(results_df$rows[i])),
              results_df$time[i]))
}

# Summary statistics
cat("\n=== SUMMARY ===\n")
cat(sprintf("Total test time: %.1f seconds\n", total_time))
cat(sprintf("Average time per test: %.1f seconds\n", mean(results_df$time, na.rm = TRUE)))
cat(sprintf("Tests passed: %d/%d\n", sum(results_df$status %in% c("SUCCESS", "SUCCESS_LIST")), nrow(results_df)))

# Key findings
cat("\n=== KEY FINDINGS ===\n")

unified_daily <- results_df[grep("unified.*daily", paste(endpoint, params))]
if (all(unified_daily$status %in% c("EMPTY", "ERROR"))) {
  cat("❌ Unified endpoint + daily granularity = NO DATA\n")
}

platform_daily <- results_df[grep("(ios|android).*daily", paste(endpoint, params))]
if (any(platform_daily$status == "SUCCESS")) {
  cat("✅ Platform-specific + daily granularity = SUCCESS\n")
}

unified_monthly <- results_df[grep("unified.*monthly", paste(endpoint, params))]
if (any(unified_monthly$status == "SUCCESS")) {
  cat("✅ Unified endpoint + monthly granularity = SUCCESS\n")
}

# Save detailed results
fwrite(results_df, "parallel_test_results.csv")
cat("\nDetailed results saved to: parallel_test_results.csv\n")

# Create a quick reference matrix
cat("\n=== QUICK REFERENCE MATRIX ===\n")
matrix_data <- results_df[, .(
  works = any(status %in% c("SUCCESS", "SUCCESS_LIST"))
), by = .(
  platform = ifelse(grepl("unified", endpoint), "Unified", 
                   ifelse(grepl("ios", endpoint), "iOS", "Android")),
  granularity = gsub(".*granularity=([^,]+).*", "\\1", params)
)]

print(dcast(matrix_data, platform ~ granularity, value.var = "works", fun.aggregate = any))

cat("\n✅ Testing complete!\n")