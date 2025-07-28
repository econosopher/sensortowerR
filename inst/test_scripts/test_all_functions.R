# Comprehensive Function Test Suite
# Tests all major functions with real API calls

library(sensortowerR)
library(dplyr)

cat("=== Testing All sensortowerR Functions ===\n\n")

# Test 1: st_app_info
cat("Test 1: st_app_info\n")
tryCatch({
  # Basic search
  result1 <- st_app_info("Candy Crush", limit = 1)
  cat(sprintf("  ✓ Basic search returned %d result(s)\n", nrow(result1)))
  
  # With all fields
  result2 <- st_app_info("Pokemon GO", limit = 1, return_all_fields = TRUE)
  cat(sprintf("  ✓ Full fields search returned %d columns\n", ncol(result2)))
  
  cat("  ✓ st_app_info working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_app_info failed: %s\n", e$message))
})

# Test 2: st_publisher_apps  
cat("\nTest 2: st_publisher_apps\n")
tryCatch({
  # Get Supercell apps
  supercell_apps <- st_publisher_apps("560c48b48ac350643900b82d")
  cat(sprintf("  ✓ Found %d Supercell apps\n", nrow(supercell_apps)))
  cat("  ✓ st_publisher_apps working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_publisher_apps failed: %s\n", e$message))
})

# Test 3: st_metrics
cat("\nTest 3: st_metrics\n")
tryCatch({
  # Test with unified app_id
  metrics1 <- st_metrics(
    app_id = "553834731",  # Candy Crush
    start_date = Sys.Date() - 7,
    end_date = Sys.Date() - 1,
    verbose = FALSE
  )
  cat(sprintf("  ✓ Unified app_id returned %d rows\n", nrow(metrics1)))
  
  # Test with platform-specific IDs
  metrics2 <- st_metrics(
    ios_app_id = "1195621598",
    android_app_id = "com.playrix.homescapes",
    start_date = Sys.Date() - 7,
    end_date = Sys.Date() - 1,
    verbose = FALSE
  )
  cat(sprintf("  ✓ Platform-specific IDs returned %d rows\n", nrow(metrics2)))
  
  # Test daily granularity (should auto-switch to platform endpoints)
  metrics3 <- st_metrics(
    app_id = "553834731",
    start_date = Sys.Date() - 7,
    end_date = Sys.Date() - 1,
    date_granularity = "daily",
    verbose = FALSE
  )
  cat(sprintf("  ✓ Daily granularity returned %d rows\n", nrow(metrics3)))
  
  cat("  ✓ st_metrics working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_metrics failed: %s\n", e$message))
})

# Test 4: st_ytd_metrics (minimal API calls)
cat("\nTest 4: st_ytd_metrics\n")
tryCatch({
  # Test with small date range to minimize API usage
  ytd_result <- st_ytd_metrics(
    unified_app_id = "553834731",
    years = 2024,
    period_start = "03-01",
    period_end = "03-02",  # Just 2 days
    metrics = "revenue",
    verbose = FALSE
  )
  cat(sprintf("  ✓ Single app/year returned %d rows\n", nrow(ytd_result)))
  
  # Test multiple apps
  multi_result <- st_ytd_metrics(
    unified_app_id = c("553834731", "1195621598"),
    years = 2024,
    period_start = "03-01", 
    period_end = "03-01",  # Just 1 day
    metrics = "downloads",
    verbose = FALSE
  )
  cat(sprintf("  ✓ Multiple apps returned %d rows\n", nrow(multi_result)))
  
  cat("  ✓ st_ytd_metrics working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_ytd_metrics failed: %s\n", e$message))
})

# Test 5: st_top_charts
cat("\nTest 5: st_top_charts\n")
tryCatch({
  # Revenue charts
  revenue_charts <- st_top_charts(
    category = 6014,  # Games
    limit = 5,
    enrich_response = FALSE  # Faster without enrichment
  )
  cat(sprintf("  ✓ Revenue charts returned %d apps\n", nrow(revenue_charts)))
  
  # Download charts
  download_charts <- st_top_charts(
    measure = "units",
    category = 6014,
    limit = 5,
    enrich_response = FALSE
  )
  cat(sprintf("  ✓ Download charts returned %d apps\n", nrow(download_charts)))
  
  cat("  ✓ st_top_charts working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_top_charts failed: %s\n", e$message))
})

# Test 6: st_category_rankings  
cat("\nTest 6: st_category_rankings\n")
tryCatch({
  # iOS rankings
  ios_rankings <- st_category_rankings(
    os = "ios",
    category = 6014,
    chart_type = "topfreeapplications",
    country = "US",
    limit = 5
  )
  cat(sprintf("  ✓ iOS rankings returned %d apps\n", nrow(ios_rankings)))
  
  # Android rankings
  android_rankings <- st_category_rankings(
    os = "android",
    category = "game",
    chart_type = "topgrossing",
    country = "US",
    limit = 5
  )
  cat(sprintf("  ✓ Android rankings returned %d apps\n", nrow(android_rankings)))
  
  cat("  ✓ st_category_rankings working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_category_rankings failed: %s\n", e$message))
})

# Test 7: st_top_publishers
cat("\nTest 7: st_top_publishers\n")
tryCatch({
  # Top publishers by revenue
  top_pubs <- st_top_publishers(
    measure = "revenue",
    category = 6014,
    time_range = "month",
    date = format(Sys.Date() - 35, "%Y-%m-01"),  # Last month
    limit = 5,
    include_apps = FALSE  # Faster
  )
  cat(sprintf("  ✓ Top publishers returned %d publishers\n", nrow(top_pubs)))
  
  cat("  ✓ st_top_publishers working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_top_publishers failed: %s\n", e$message))
})

# Test 8: st_app_details
cat("\nTest 8: st_app_details\n")
tryCatch({
  # Get details for multiple apps
  details <- st_app_details(
    app_ids = c("553834731", "1195621598"),
    os = "ios"
  )
  cat(sprintf("  ✓ App details returned %d apps\n", nrow(details)))
  
  cat("  ✓ st_app_details working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_app_details failed: %s\n", e$message))
})

# Test 9: st_game_summary
cat("\nTest 9: st_game_summary\n")
tryCatch({
  # Game market summary
  game_summary <- st_game_summary(
    categories = 7001,  # Action games
    os = "ios",
    countries = "US",
    date_granularity = "monthly",
    start_date = format(Sys.Date() - 35, "%Y-%m-01"),
    end_date = format(Sys.Date() - 5, "%Y-%m-%d")
  )
  cat(sprintf("  ✓ Game summary returned %d rows\n", nrow(game_summary)))
  
  cat("  ✓ st_game_summary working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_game_summary failed: %s\n", e$message))
})

# Test 10: st_sales_report
cat("\nTest 10: st_sales_report\n")
tryCatch({
  # Test batch capabilities
  sales_report <- st_sales_report(
    app_ids = c("553834731", "1195621598"),  # Multiple apps
    countries = "US",
    start_date = Sys.Date() - 7,
    end_date = Sys.Date() - 1,
    date_granularity = "daily"
  )
  cat(sprintf("  ✓ Sales report returned %d rows for %d apps\n", 
              nrow(sales_report), length(unique(sales_report$app_id))))
  
  cat("  ✓ st_sales_report working correctly\n")
}, error = function(e) {
  cat(sprintf("  ✗ st_sales_report failed: %s\n", e$message))
})

# Summary
cat("\n=== Test Summary ===\n")
cat("All major functions have been tested.\n")
cat("Check for any ✗ marks above to identify issues.\n")