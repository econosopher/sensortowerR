# Test the unified st_metrics function

library(sensortowerR)

cat("=== Testing Unified st_metrics Function ===\n\n")

# Test 1: Simple usage with auto-detection
cat("Test 1: Auto-detection with iOS app ID\n")
metrics1 <- st_metrics(
  app_id = "1195621598",  # Homescapes iOS
  start_date = Sys.Date() - 7,
  end_date = Sys.Date() - 1,
  verbose = TRUE
)
cat(sprintf("  Result: %d rows\n\n", nrow(metrics1)))

# Test 2: Android app ID
cat("Test 2: Auto-detection with Android package name\n")
metrics2 <- st_metrics(
  app_id = "com.playrix.homescapes",
  start_date = Sys.Date() - 7,
  end_date = Sys.Date() - 1,
  verbose = TRUE
)
cat(sprintf("  Result: %d rows\n\n", nrow(metrics2)))

# Test 3: Both platforms for unified view
cat("Test 3: Both platforms combined\n")
metrics3 <- st_metrics(
  app_id = "1195621598",
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes",
  start_date = Sys.Date() - 7,
  end_date = Sys.Date() - 1,
  combine_platforms = TRUE,
  verbose = TRUE
)
cat(sprintf("  Result: %d rows\n", nrow(metrics3)))
if (nrow(metrics3) > 0) {
  cat(sprintf("  Total revenue: $%s\n", format(sum(metrics3$revenue), big.mark = ",")))
}

# Test 4: Keep platforms separate
cat("\nTest 4: Both platforms separate\n")
metrics4 <- st_metrics(
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes",
  start_date = Sys.Date() - 7,
  end_date = Sys.Date() - 1,
  combine_platforms = FALSE,
  verbose = FALSE
)
cat(sprintf("  Result: %d rows\n", nrow(metrics4)))
if (nrow(metrics4) > 0 && "platform" %in% names(metrics4)) {
  platform_counts <- table(metrics4$platform)
  cat(sprintf("  iOS rows: %d\n", platform_counts["iOS"]))
  cat(sprintf("  Android rows: %d\n", platform_counts["Android"]))
}

# Test 5: Deprecated parameter handling
cat("\nTest 5: Deprecated unified_app_id parameter\n")
suppressWarnings({
  metrics5 <- st_metrics(
    unified_app_id = "1195621598",  # Old parameter name
    start_date = Sys.Date() - 7,
    end_date = Sys.Date() - 1,
    verbose = FALSE
  )
})
cat(sprintf("  Result: %d rows (should work with warning)\n", nrow(metrics5)))

# Test 6: Force unified endpoint (will fail for daily)
cat("\nTest 6: Force unified endpoint (expected to fail)\n")
metrics6 <- st_metrics(
  app_id = "1195621598",
  auto_platform_fetch = FALSE,
  start_date = Sys.Date() - 7,
  end_date = Sys.Date() - 1,
  verbose = FALSE
)
cat(sprintf("  Result: %d rows (expected 0)\n", nrow(metrics6)))

# Test 7: Weekly data (could potentially use unified)
cat("\nTest 7: Weekly granularity\n")
metrics7 <- st_metrics(
  app_id = "1195621598",
  date_granularity = "weekly",
  start_date = Sys.Date() - 30,
  end_date = Sys.Date() - 1,
  verbose = TRUE
)
cat(sprintf("  Result: %d rows\n", nrow(metrics7)))

cat("\n✅ All tests complete!\n")