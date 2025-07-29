# Manual API functionality tests
# Run this script to verify all functions work correctly with real API data

library(sensortowerR)
library(dplyr)

# Check if auth token is available
if (Sys.getenv("SENSORTOWER_AUTH_TOKEN") == "") {
  stop("Please set SENSORTOWER_AUTH_TOKEN environment variable")
}

# Test results will be stored here
test_results <- list()

cat("\n===== TESTING SENSORTOWER R PACKAGE API ENDPOINTS =====\n\n")

# 1. Test st_metrics - Basic functionality
cat("1. Testing st_metrics (basic iOS data)...\n")
test_results$st_metrics_ios <- tryCatch({
  result <- st_metrics(
    os = "ios",
    ios_app_id = "553834731",  # Candy Crush
    countries = "US",
    date_granularity = "monthly",
    start_date = "2024-01-01",
    end_date = "2024-03-31"
  )
  
  cat("  ✓ Retrieved", nrow(result), "records\n")
  cat("  ✓ Columns:", paste(names(result), collapse = ", "), "\n")
  cat("  ✓ Total revenue: $", format(sum(result$revenue), big.mark = ","), "\n")
  cat("  ✓ Total downloads:", format(sum(result$downloads), big.mark = ","), "\n")
  
  list(success = TRUE, rows = nrow(result), data = result)
}, error = function(e) {
  cat("  ✗ ERROR:", e$message, "\n")
  list(success = FALSE, error = e$message)
})

# 2. Test unified data
cat("\n2. Testing st_metrics (unified iOS + Android)...\n")
test_results$st_metrics_unified <- tryCatch({
  result <- st_metrics(
    os = "unified",
    ios_app_id = "553834731",
    android_app_id = "com.king.candycrushsaga",
    countries = "US",
    date_granularity = "monthly",
    start_date = "2024-01-01",
    end_date = "2024-01-31"
  )
  
  cat("  ✓ Retrieved", nrow(result), "records\n")
  cat("  ✓ Combined revenue: $", format(sum(result$revenue), big.mark = ","), "\n")
  cat("  ✓ Combined downloads:", format(sum(result$downloads), big.mark = ","), "\n")
  
  list(success = TRUE, rows = nrow(result), data = result)
}, error = function(e) {
  cat("  ✗ ERROR:", e$message, "\n")
  list(success = FALSE, error = e$message)
})

# 3. Test st_batch_metrics
cat("\n3. Testing st_batch_metrics (multiple apps)...\n")
test_results$st_batch_metrics <- tryCatch({
  apps <- c("553834731", "1195621598")  # Candy Crush, Homescapes
  
  result <- st_batch_metrics(
    os = "ios",
    app_list = apps,
    metrics = c("revenue", "downloads"),
    date_range = list(start_date = "2024-01-01", end_date = "2024-01-31"),
    countries = "US",
    granularity = "monthly"
  )
  
  cat("  ✓ Retrieved", nrow(result), "records\n")
  cat("  ✓ Apps processed:", length(unique(result$original_id)), "\n")
  cat("  ✓ Metrics:", paste(unique(result$metric), collapse = ", "), "\n")
  
  # Show summary by app
  summary <- result %>%
    group_by(original_id, metric) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")
  
  print(summary)
  
  list(success = TRUE, rows = nrow(result), data = result)
}, error = function(e) {
  cat("  ✗ ERROR:", e$message, "\n")
  list(success = FALSE, error = e$message)
})

# 4. Test st_yoy_metrics
cat("\n4. Testing st_yoy_metrics (year-over-year comparison)...\n")
test_results$st_yoy_metrics <- tryCatch({
  result <- st_yoy_metrics(
    os = "ios",
    ios_app_id = "553834731",
    years = c(2023, 2024),
    period_start = "01-01",
    period_end = "01-31",
    countries = "US",
    metrics = c("revenue", "downloads")
  )
  
  cat("  ✓ Retrieved", nrow(result), "records\n")
  cat("  ✓ Years:", paste(unique(result$year), collapse = ", "), "\n")
  
  # Show YoY changes
  yoy_summary <- result %>%
    filter(!is.na(yoy_change)) %>%
    select(year, metric, value, yoy_change, yoy_change_absolute)
  
  if (nrow(yoy_summary) > 0) {
    cat("\n  Year-over-Year Changes:\n")
    print(yoy_summary)
  }
  
  list(success = TRUE, rows = nrow(result), data = result)
}, error = function(e) {
  cat("  ✗ ERROR:", e$message, "\n")
  list(success = FALSE, error = e$message)
})

# 5. Test st_top_charts
cat("\n5. Testing st_top_charts (top revenue games)...\n")
test_results$st_top_charts <- tryCatch({
  result <- st_top_charts(
    os = "ios",
    comparison_attribute = "absolute",
    time_range = "month",
    measure = "revenue",
    date = "2024-01-01",
    category = 6014,  # Games
    regions = "US",
    limit = 10
  )
  
  cat("  ✓ Retrieved top", nrow(result), "apps\n")
  
  # Show top 5
  if (nrow(result) > 0) {
    cat("\n  Top 5 by revenue:\n")
    top5 <- result %>% 
      select(any_of(c("rank", "current_rank", "app_id", "app_name", 
                      "revenue_absolute", "current_revenue_value"))) %>%
      head(5)
    print(top5)
  }
  
  list(success = TRUE, rows = nrow(result), data = result)
}, error = function(e) {
  cat("  ✗ ERROR:", e$message, "\n")
  list(success = FALSE, error = e$message)
})

# 6. Test st_top_publishers
cat("\n6. Testing st_top_publishers...\n")
test_results$st_top_publishers <- tryCatch({
  result <- st_top_publishers(
    measure = "revenue",
    os = "unified",
    category = 6014,  # Games
    date = "2024-01-01",
    country = "US",
    limit = 5,
    include_apps = TRUE
  )
  
  cat("  ✓ Retrieved top", nrow(result), "publishers\n")
  
  # Show results
  if (nrow(result) > 0) {
    cat("\n  Top publishers:\n")
    print(result %>% select(rank, publisher_name, any_of(c("revenue_absolute", "revenue_usd"))))
  }
  
  list(success = TRUE, rows = nrow(result), data = result)
}, error = function(e) {
  cat("  ✗ ERROR:", e$message, "\n")
  list(success = FALSE, error = e$message)
})

# 7. Test st_app_info
cat("\n7. Testing st_app_info (app search)...\n")
test_results$st_app_info <- tryCatch({
  result <- st_app_info(
    term = "Candy Crush",
    app_store = "unified",
    entity_type = "app",
    limit = 5
  )
  
  cat("  ✓ Found", nrow(result), "apps\n")
  
  if (nrow(result) > 0) {
    cat("\n  Apps found:\n")
    print(result %>% select(unified_app_name, ios_app_id, android_app_id, 
                           publisher_name) %>% head(3))
  }
  
  list(success = TRUE, rows = nrow(result), data = result)
}, error = function(e) {
  cat("  ✗ ERROR:", e$message, "\n")
  list(success = FALSE, error = e$message)
})

# 8. Test st_game_summary
cat("\n8. Testing st_game_summary...\n")
test_results$st_game_summary <- tryCatch({
  result <- st_game_summary(
    os = "unified",
    app_id = "553834731",  # Will be resolved to unified ID
    regions = "US"
  )
  
  cat("  ✓ Retrieved game summary\n")
  cat("  ✓ Columns:", paste(names(result), collapse = ", "), "\n")
  
  # Show key metrics if available
  if (nrow(result) > 0) {
    revenue_cols <- grep("revenue", names(result), value = TRUE, ignore.case = TRUE)
    download_cols <- grep("download", names(result), value = TRUE, ignore.case = TRUE)
    
    if (length(revenue_cols) > 0) {
      cat("  ✓ Revenue metrics found:", length(revenue_cols), "\n")
    }
    if (length(download_cols) > 0) {
      cat("  ✓ Download metrics found:", length(download_cols), "\n")
    }
  }
  
  list(success = TRUE, rows = nrow(result), data = result)
}, error = function(e) {
  cat("  ✗ ERROR:", e$message, "\n")
  list(success = FALSE, error = e$message)
})

# Summary
cat("\n\n===== TEST SUMMARY =====\n")
success_count <- sum(sapply(test_results, function(x) x$success))
total_count <- length(test_results)

cat("Successful tests:", success_count, "/", total_count, "\n")

if (success_count < total_count) {
  cat("\nFailed tests:\n")
  for (name in names(test_results)) {
    if (!test_results[[name]]$success) {
      cat("  -", name, "\n")
    }
  }
}

cat("\nData retrieval summary:\n")
for (name in names(test_results)) {
  if (test_results[[name]]$success && !is.null(test_results[[name]]$rows)) {
    cat("  -", name, ":", test_results[[name]]$rows, "rows\n")
  }
}

# Save results for inspection
save(test_results, file = "test_results.RData")
cat("\nDetailed results saved to test_results.RData\n")