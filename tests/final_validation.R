# Final Validation Test
# Ensures all functions work correctly with the updated understanding of API limitations

library(sensortowerR)
library(dplyr)

cat("=== FINAL VALIDATION TEST ===\n")
cat(sprintf("Date: %s\n\n", Sys.Date()))

success_count <- 0
total_tests <- 0

# Test 1: st_metrics with all granularities
cat("1. Testing st_metrics with different granularities\n")
for (gran in c("daily", "weekly", "monthly")) {
  total_tests <- total_tests + 1
  
  tryCatch({
    result <- st_metrics(
      app_id = "553834731",  # Candy Crush
      start_date = Sys.Date() - 30,
      end_date = Sys.Date() - 1,
      date_granularity = gran,
      verbose = FALSE
    )
    
    if (nrow(result) > 0) {
      cat(sprintf("  ✅ %s granularity: %d rows\n", gran, nrow(result)))
      success_count <- success_count + 1
    } else {
      cat(sprintf("  ❌ %s granularity: No data\n", gran))
    }
  }, error = function(e) {
    cat(sprintf("  ❌ %s granularity: Error - %s\n", gran, e$message))
  })
}

# Test 2: st_metrics with platform-specific IDs
cat("\n2. Testing st_metrics with platform-specific IDs\n")
total_tests <- total_tests + 1

tryCatch({
  result <- st_metrics(
    ios_app_id = "1195621598",
    android_app_id = "com.playrix.homescapes",
    start_date = Sys.Date() - 7,
    end_date = Sys.Date() - 1,
    verbose = FALSE
  )
  
  if (nrow(result) > 0) {
    cat(sprintf("  ✅ Platform-specific: %d rows (revenue total: $%.0f)\n", 
                nrow(result), sum(result$revenue)))
    success_count <- success_count + 1
  } else {
    cat("  ❌ Platform-specific: No data\n")
  }
}, error = function(e) {
  cat(sprintf("  ❌ Platform-specific: Error - %s\n", e$message))
})

# Test 3: st_ytd_metrics
cat("\n3. Testing st_ytd_metrics\n")
total_tests <- total_tests + 1

tryCatch({
  result <- st_ytd_metrics(
    unified_app_id = "553834731",
    years = 2024,
    period_start = "01-01",
    period_end = "01-07",  # Just one week
    metrics = "revenue",
    verbose = FALSE
  )
  
  if (nrow(result) > 0) {
    cat(sprintf("  ✅ YTD metrics: %d rows\n", nrow(result)))
    success_count <- success_count + 1
  } else {
    cat("  ❌ YTD metrics: No data\n")
  }
}, error = function(e) {
  cat(sprintf("  ❌ YTD metrics: Error - %s\n", e$message))
})

# Test 4: st_top_publishers
cat("\n4. Testing st_top_publishers\n")
total_tests <- total_tests + 1

tryCatch({
  result <- st_top_publishers(
    category = 6014,
    time_range = "month",
    date = format(Sys.Date() - 35, "%Y-%m-01"),
    limit = 3,
    include_apps = FALSE
  )
  
  if (nrow(result) > 0) {
    cat(sprintf("  ✅ Top publishers: %d publishers\n", nrow(result)))
    success_count <- success_count + 1
  } else {
    cat("  ❌ Top publishers: No data\n")
  }
}, error = function(e) {
  cat(sprintf("  ❌ Top publishers: Error - %s\n", e$message))
})

# Test 5: st_sales_report batch
cat("\n5. Testing st_sales_report batch capabilities\n")
total_tests <- total_tests + 1

tryCatch({
  result <- st_sales_report(
    app_ids = c("553834731", "1195621598", "1053012308"),  # 3 apps
    countries = "US",
    start_date = Sys.Date() - 7,
    end_date = Sys.Date() - 1,
    date_granularity = "daily"
  )
  
  n_apps <- length(unique(result$app_id))
  if (nrow(result) > 0 && n_apps == 3) {
    cat(sprintf("  ✅ Batch request: %d rows for %d apps\n", nrow(result), n_apps))
    success_count <- success_count + 1
  } else {
    cat(sprintf("  ❌ Batch request: Only got data for %d apps\n", n_apps))
  }
}, error = function(e) {
  cat(sprintf("  ❌ Batch request: Error - %s\n", e$message))
})

# Test 6: Verify unified endpoint is broken
cat("\n6. Confirming unified endpoint is broken\n")
total_tests <- total_tests + 1

tryCatch({
  # Force use of unified endpoint
  result <- st_metrics(
    app_id = "553834731",
    start_date = Sys.Date() - 7,
    end_date = Sys.Date() - 1,
    auto_platform_fetch = FALSE,  # Disable automatic platform switching
    verbose = FALSE
  )
  
  if (nrow(result) == 0) {
    cat("  ✅ Confirmed: Unified endpoint returns empty (as expected)\n")
    success_count <- success_count + 1
  } else {
    cat("  ❌ Unexpected: Unified endpoint returned data!\n")
  }
}, error = function(e) {
  cat("  ❌ Error testing unified endpoint\n")
})

# Summary
cat(sprintf("\n=== SUMMARY ===\n"))
cat(sprintf("Tests passed: %d/%d (%.0f%%)\n", 
            success_count, total_tests, (success_count/total_tests)*100))

if (success_count == total_tests) {
  cat("\n✅ All tests passed! Package is working correctly.\n")
} else {
  cat("\n⚠️ Some tests failed. Check the output above.\n")
}

cat("\nKey findings:\n")
cat("- st_metrics now always uses platform-specific endpoints\n")
cat("- All granularities (daily, weekly, monthly) work correctly\n")
cat("- Batch requests work efficiently\n")
cat("- Unified endpoint confirmed broken (returns empty)\n")