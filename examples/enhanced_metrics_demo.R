# Enhanced Custom Metrics Demo
# This script demonstrates the powerful custom metrics extraction from st_top_active_users

library(devtools)
load_all(".")

cat("=== Enhanced Custom Metrics Extraction Demo ===\n\n")

# Get top role-playing games with enhanced metrics
cat("Fetching top role-playing games with enhanced analytics...\n")
rpg_games <- st_top_active_users(
  comparison_attribute = "absolute",
  time_range = "month",
  measure = "MAU",
  category = "7014",  # Role Playing
  limit = 5
)

cat("✅ Retrieved", nrow(rpg_games), "games with", ncol(rpg_games), "total columns\n\n")

# Show the clean metrics that are now easily accessible
clean_metrics <- names(rpg_games)[!startsWith(names(rpg_games), 'entities.') & 
                                  !startsWith(names(rpg_games), 'aggregate_tags.')]

performance_metrics <- clean_metrics[grepl('downloads_|revenue_|dau_|wau_|mau_', clean_metrics)]
retention_metrics <- clean_metrics[grepl('retention_', clean_metrics)]
business_metrics <- clean_metrics[grepl('rpd_|release_date', clean_metrics)]

cat("🎯 PERFORMANCE METRICS (easily accessible):\n")
for(metric in performance_metrics) {
  cat("  -", metric, "\n")
}

cat("\n📈 RETENTION METRICS:\n")
for(metric in retention_metrics) {
  cat("  -", metric, "\n") 
}

cat("\n💰 BUSINESS METRICS:\n")
for(metric in business_metrics) {
  cat("  -", metric, "\n")
}

cat("\n=== SAMPLE DATA ===\n")
if(nrow(rpg_games) > 0 && "unified_app_name" %in% names(rpg_games)) {
  sample_game <- rpg_games[!is.na(rpg_games$unified_app_name), ][1, ]
  cat("Sample game:", sample_game$unified_app_name, "\n")
  
  # Show some key metrics
  if("downloads_180d_ww" %in% names(sample_game) && !is.na(sample_game$downloads_180d_ww)) {
    cat("  📥 Downloads (180d):", format(sample_game$downloads_180d_ww, big.mark=","), "\n")
  }
  if("revenue_180d_ww" %in% names(sample_game) && !is.na(sample_game$revenue_180d_ww)) {
    cat("  💰 Revenue (180d): $", format(sample_game$revenue_180d_ww, big.mark=","), "\n")
  }
  if("retention_1d_us" %in% names(sample_game) && !is.na(sample_game$retention_1d_us)) {
    cat("  📊 Day 1 Retention:", paste0(round(sample_game$retention_1d_us * 100, 1), "%"), "\n")
  }
  if("rpd_alltime_us" %in% names(sample_game) && !is.na(sample_game$rpd_alltime_us)) {
    cat("  💵 RPD (All Time): $", format(round(sample_game$rpd_alltime_us, 2), nsmall=2), "\n")
  }
}

cat("\n=== BEFORE vs AFTER ===\n")
cat("BEFORE: Accessing revenue data required complex column names\n")
cat("❌ result$`entities.custom_tags.Last 180 Days Revenue (WW)`\n")
cat("❌ result$`aggregate_tags.Last 180 Days Revenue (WW)`\n\n")

cat("AFTER: Clean, intuitive column names\n") 
cat("✅ result$revenue_180d_ww\n")
cat("✅ result$downloads_180d_ww\n")
cat("✅ result$retention_1d_us\n")
cat("✅ result$rpd_alltime_us\n\n")

cat("🎉 Much easier for analysis and visualization!\n")
cat("📊 Perfect for business intelligence dashboards!\n") 