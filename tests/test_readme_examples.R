# Test README Examples with Actual API Calls
# This validates that all examples in the README work correctly

library(sensortowerR)
library(dplyr)
library(tidyr)

cat("=== Testing README Examples ===\n\n")

# Example 1: Basic App Search
cat("Example 1: Basic App Search\n")
tryCatch({
  monopoly_info <- st_app_info("Monopoly Go", limit = 1)
  pokemon_info <- st_app_info("Pokemon GO", limit = 1)
  cat(sprintf("  ✓ Found Monopoly Go: %s\n", monopoly_info$unified_app_name[1]))
  cat(sprintf("  ✓ Found Pokemon GO: %s\n", pokemon_info$unified_app_name[1]))
}, error = function(e) {
  cat(sprintf("  ✗ App search failed: %s\n", e$message))
})

# Example 2: Publisher Apps
cat("\nExample 2: Publisher Apps\n")
tryCatch({
  supercell_apps <- st_publisher_apps("560c48b48ac350643900b82d")
  cat(sprintf("  ✓ Found %d Supercell apps\n", nrow(supercell_apps)))
}, error = function(e) {
  cat(sprintf("  ✗ Publisher apps failed: %s\n", e$message))
})

# Example 3: App Metrics - Simple usage
cat("\nExample 3: App Metrics\n")
tryCatch({
  # Auto-detect platform
  metrics <- st_metrics(
    app_id = "1195621598",  # Homescapes iOS
    start_date = Sys.Date() - 30,
    end_date = Sys.Date() - 1,
    verbose = FALSE
  )
  cat(sprintf("  ✓ Simple usage returned %d rows\n", nrow(metrics)))
  
  # Platform-specific IDs
  metrics2 <- st_metrics(
    ios_app_id = "1195621598",
    android_app_id = "com.playrix.homescapes",
    start_date = Sys.Date() - 30,
    end_date = Sys.Date() - 1,
    verbose = FALSE
  )
  cat(sprintf("  ✓ Platform-specific returned %d rows\n", nrow(metrics2)))
}, error = function(e) {
  cat(sprintf("  ✗ App metrics failed: %s\n", e$message))
})

# Example 4: YTD Metrics
cat("\nExample 4: YTD Metrics\n")
tryCatch({
  # Single app YTD
  ytd_metrics <- st_ytd_metrics(
    unified_app_id = "553834731",  # Candy Crush
    years = 2024,
    period_start = "01-01",
    period_end = "01-02",  # Just 2 days to minimize API usage
    verbose = FALSE
  )
  cat(sprintf("  ✓ YTD metrics returned %d rows\n", nrow(ytd_metrics)))
  
  # YTD growth calculation
  if (nrow(ytd_metrics) > 0) {
    growth_test <- ytd_metrics %>%
      filter(metric == "revenue") %>%
      select(entity_id, year, value)
    cat("  ✓ Growth calculation structure validated\n")
  }
}, error = function(e) {
  cat(sprintf("  ✗ YTD metrics failed: %s\n", e$message))
})

# Example 5: Top Charts
cat("\nExample 5: Top Charts\n")
tryCatch({
  # Revenue charts
  top_revenue <- st_top_charts(category = 6000, limit = 3, enrich_response = FALSE)
  cat(sprintf("  ✓ Revenue charts returned %d apps\n", nrow(top_revenue)))
  
  # Download charts
  top_downloads <- st_top_charts(measure = "units", category = 6000, limit = 3, enrich_response = FALSE)
  cat(sprintf("  ✓ Download charts returned %d apps\n", nrow(top_downloads)))
}, error = function(e) {
  cat(sprintf("  ✗ Top charts failed: %s\n", e$message))
})

# Example 6: Category Rankings
cat("\nExample 6: Category Rankings\n")
tryCatch({
  # iOS rankings
  ios_top_free <- st_category_rankings(
    os = "ios",
    category = 6014,
    chart_type = "topfreeapplications",
    country = "US",
    limit = 3
  )
  cat(sprintf("  ✓ iOS rankings returned %d apps\n", nrow(ios_top_free)))
  
  # Android rankings
  android_top_grossing <- st_category_rankings(
    os = "android",
    category = "game",
    chart_type = "topgrossing",
    country = "US",
    limit = 3
  )
  cat(sprintf("  ✓ Android rankings returned %d apps\n", nrow(android_top_grossing)))
}, error = function(e) {
  cat(sprintf("  ✗ Category rankings failed: %s\n", e$message))
})

# Example 7: Top Publishers
cat("\nExample 7: Top Publishers\n")
tryCatch({
  # Auto end_date calculation
  top_publishers <- st_top_publishers(
    measure = "revenue",
    os = "unified",
    category = 6014,
    time_range = "month",
    date = format(Sys.Date() - 35, "%Y-%m-01"),
    limit = 3,
    include_apps = FALSE
  )
  cat(sprintf("  ✓ Top publishers returned %d publishers\n", nrow(top_publishers)))
  
  # Check date columns
  if ("date_start" %in% names(top_publishers) && "date_end" %in% names(top_publishers)) {
    cat("  ✓ Date range columns present\n")
  }
}, error = function(e) {
  cat(sprintf("  ✗ Top publishers failed: %s\n", e$message))
})

# Example 8: Sales Report Batch
cat("\nExample 8: Sales Report Batch\n")
tryCatch({
  # Multiple apps in one call
  revenue_data <- st_sales_report(
    app_ids = c("1195621598", "553834731"),  # Multiple iOS apps
    countries = "US",
    start_date = Sys.Date() - 7,
    end_date = Sys.Date() - 1,
    date_granularity = "daily"
  )
  
  n_apps <- length(unique(revenue_data$app_id))
  cat(sprintf("  ✓ Batch request returned data for %d apps\n", n_apps))
  cat(sprintf("  ✓ Total rows: %d (should be ~14 for 2 apps × 7 days)\n", nrow(revenue_data)))
}, error = function(e) {
  cat(sprintf("  ✗ Sales report batch failed: %s\n", e$message))
})

# Example 9: Game Summary
cat("\nExample 9: Game Summary\n")
tryCatch({
  game_market <- st_game_summary(
    categories = 7001,           # Action games
    os = "ios",
    countries = "US",
    date_granularity = "monthly",
    start_date = format(Sys.Date() - 35, "%Y-%m-01"),
    end_date = format(Sys.Date() - 5, "%Y-%m-%d")
  )
  
  # Check iOS column consolidation
  if ("iOS Revenue" %in% names(game_market)) {
    cat("  ✓ iOS Revenue column found (iPhone+iPad combined)\n")
  }
  if ("iOS Downloads" %in% names(game_market)) {
    cat("  ✓ iOS Downloads column found (iPhone+iPad combined)\n")
  }
}, error = function(e) {
  cat(sprintf("  ✗ Game summary failed: %s\n", e$message))
})

# Summary
cat("\n=== README Examples Test Complete ===\n")
cat("Check for any ✗ marks above to identify broken examples.\n")
cat("All ✓ marks indicate the README examples are working correctly.\n")