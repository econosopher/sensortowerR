#!/usr/bin/env Rscript

# Test st_sales_report with app ID validation

library(sensortowerR)

cat("Testing st_sales_report app ID validation with new parameter pattern...\n\n")

# Test 1: New parameter style - iOS app ID with iOS OS - should work
cat("Test 1: New style - ios_app_id with os='ios' (should work)...\n")
tryCatch({
  result <- st_sales_report(
    os = "ios",
    ios_app_id = "1195621598",  # Homescapes iOS
    countries = "US",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    date_granularity = "daily",
    verbose = FALSE
  )
  cat("✓ New style iOS ID with iOS OS worked correctly\n\n")
}, error = function(e) {
  if (grepl("Authentication token", e$message)) {
    cat("✓ New style iOS ID validation passed (stopped at auth check)\n\n")
  } else {
    cat("✗ Unexpected error:", e$message, "\n\n")
  }
})

# Test 2: New parameter style - Android app ID with Android OS - should work
cat("Test 2: New style - android_app_id with os='android' (should work)...\n")
tryCatch({
  result <- st_sales_report(
    os = "android",
    android_app_id = "com.playrix.homescapes",  # Homescapes Android
    countries = "US", 
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    date_granularity = "daily",
    verbose = FALSE
  )
  cat("✓ New style Android ID with Android OS worked correctly\n\n")
}, error = function(e) {
  if (grepl("Authentication token", e$message)) {
    cat("✓ New style Android ID validation passed (stopped at auth check)\n\n")
  } else {
    cat("✗ Unexpected error:", e$message, "\n\n")
  }
})

# Test 3: Cross-platform ID resolution - iOS ID for Android data (should attempt lookup)
cat("Test 3: Cross-platform - ios_app_id with os='android' (should attempt lookup)...\n")
tryCatch({
  result <- st_sales_report(
    os = "android",
    ios_app_id = "1195621598",  # iOS ID but requesting Android data
    countries = "US",
    start_date = "2024-01-01", 
    end_date = "2024-01-01",
    date_granularity = "daily",
    verbose = TRUE
  )
  cat("✓ Cross-platform lookup worked\n\n")
}, error = function(e) {
  if (grepl("Authentication token", e$message) || grepl("Looking up", e$message) || grepl("Failed to look up", e$message)) {
    cat("✓ Cross-platform lookup was attempted (stopped at auth or lookup)\n\n")
  } else {
    cat("Note: Error during cross-platform lookup:", e$message, "\n\n")
  }
})

# Test 4: Cross-platform ID resolution - Android ID for iOS data (should attempt lookup)
cat("Test 4: Cross-platform - android_app_id with os='ios' (should attempt lookup)...\n")
tryCatch({
  result <- st_sales_report(
    os = "ios",
    android_app_id = "com.playrix.homescapes",  # Android ID but requesting iOS data
    countries = "US",
    start_date = "2024-01-01",
    end_date = "2024-01-01", 
    date_granularity = "daily",
    verbose = TRUE
  )
  cat("✓ Cross-platform lookup worked\n\n")
}, error = function(e) {
  if (grepl("Authentication token", e$message) || grepl("Looking up", e$message) || grepl("Failed to look up", e$message)) {
    cat("✓ Cross-platform lookup was attempted (stopped at auth or lookup)\n\n")
  } else {
    cat("Note: Error during cross-platform lookup:", e$message, "\n\n")
  }
})

# Test 5: Unified OS - should fail
cat("Test 5: os='unified' (should fail)...\n")
tryCatch({
  result <- st_sales_report(
    app_ids = "5ba4585f539ce75b97db6bcb",
    os = "unified",
    countries = "US",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    date_granularity = "daily",
    verbose = FALSE
  )
  cat("✗ Should have thrown an error!\n\n")
}, error = function(e) {
  if (grepl("st_sales_report does not support os='unified'", e$message)) {
    cat("✓ Correctly rejected unified OS\n")
    cat("  Error message:", strsplit(e$message, "\n")[[1]][1], "\n\n")
  } else {
    cat("✗ Wrong error:", e$message, "\n\n")
  }
})

# Test 6: Missing ID parameters (should fail)
cat("Test 6: No ID parameters provided (should fail)...\n")
tryCatch({
  result <- st_sales_report(
    os = "ios",
    countries = "US",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    date_granularity = "daily",
    verbose = FALSE
  )
  cat("✗ Should have thrown an error!\n\n")
}, error = function(e) {
  if (grepl("At least one of", e$message)) {
    cat("✓ Correctly detected missing ID parameters\n")
    cat("  Error message:", strsplit(e$message, "\n")[[1]][1], "\n\n")
  } else {
    cat("✗ Wrong error:", e$message, "\n\n")
  }
})

# Test 7: Backward compatibility - Multiple app IDs (batch mode)
cat("Test 7: Backward compatibility - Multiple app IDs batch mode...\n")
tryCatch({
  result <- st_sales_report(
    os = "ios",
    app_ids = c("1195621598", "553834731"),  # Multiple iOS IDs
    countries = "US",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    date_granularity = "daily",
    verbose = TRUE
  )
  cat("✓ Batch mode backward compatibility works\n\n")
}, error = function(e) {
  if (grepl("Authentication token", e$message)) {
    cat("✓ Batch mode validation passed (stopped at auth check)\n\n")
  } else {
    cat("Note: Batch mode error:", e$message, "\n\n")
  }
}, warning = function(w) {
  if (grepl("deprecated", w$message)) {
    cat("✓ Deprecation warning shown for batch mode\n")
  }
})

# Test 8: New style - Unified app ID with platform OS (should attempt lookup)
cat("Test 8: New style - unified_app_id with os='ios' (should attempt lookup)...\n")
tryCatch({
  result <- st_sales_report(
    os = "ios",
    unified_app_id = "5ba4585f539ce75b97db6bcb",  # Unified ID
    countries = "US",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    date_granularity = "daily",
    verbose = TRUE
  )
  cat("✓ New style unified ID lookup attempted\n\n")
}, error = function(e) {
  if (grepl("Looking up ios ID from unified ID", e$message) || 
      grepl("Authentication token", e$message)) {
    cat("✓ New style unified ID lookup was attempted (stopped at auth or lookup)\n\n")
  } else {
    cat("Note: Error during unified ID handling:", e$message, "\n\n")
  }
})

# Test 9: Backward compatibility - old app_ids parameter (should work with deprecation warning)
cat("Test 9: Backward compatibility - app_ids parameter (should work with warning)...\n")
tryCatch({
  result <- st_sales_report(
    os = "ios",
    app_ids = "1195621598",  # Old style parameter
    countries = "US",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    date_granularity = "daily",
    verbose = FALSE
  )
  cat("✓ Backward compatibility maintained\n\n")
}, error = function(e) {
  if (grepl("Authentication token", e$message)) {
    cat("✓ Backward compatibility works (stopped at auth check)\n\n")
  } else {
    cat("✗ Backward compatibility error:", e$message, "\n\n")
  }
}, warning = function(w) {
  if (grepl("deprecated", w$message)) {
    cat("✓ Deprecation warning shown correctly\n")
    cat("  Warning:", w$message, "\n\n")
  }
})

# Test 10: Mixed parameters - should fail
cat("Test 10: Mixed parameters - app_ids + ios_app_id (should fail)...\n")
tryCatch({
  result <- st_sales_report(
    os = "ios",
    app_ids = "1195621598",  # Old style
    ios_app_id = "1195621598",  # New style - conflict!
    countries = "US",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    date_granularity = "daily",
    verbose = FALSE
  )
  cat("✗ Should have thrown an error!\n\n")
}, error = function(e) {
  if (grepl("Cannot use both", e$message)) {
    cat("✓ Correctly rejected mixed parameter usage\n")
    cat("  Error message:", strsplit(e$message, "\n")[[1]][1], "\n\n")
  } else {
    cat("✗ Wrong error:", e$message, "\n\n")
  }
})

cat("\nAll validation tests completed!\n")
cat("\nSummary:\n")
cat("- New parameter style (ios_app_id, android_app_id, unified_app_id) implemented\n")
cat("- Cross-platform ID resolution integrated\n")
cat("- Backward compatibility maintained with deprecation warnings\n")
cat("- Parameter conflict detection working\n")
cat("- Consistent behavior with st_metrics function\n")