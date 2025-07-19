# sensortowerR - Simple Demo with Improved Defaults
# This demonstrates how much simpler the API calls are with sensible defaults

# Load the package
library(devtools)
load_all(".")

cat("=== sensortowerR: Simplified API Demo ===\n\n")

# Example 1: Get app info (now defaults to limit=20 instead of 100)
cat("1. Getting app info for Pokemon games:\n")
apps <- st_app_info("Pokemon")
cat("   Found", nrow(apps), "apps (using default limit=20)\n\n")

# Example 2: Get app metrics (now with automatic date range)
cat("2. Getting metrics for Pokemon GO (automatic date range):\n")
metrics <- st_metrics("602c795c912b51622f233ffe")
cat("   Returned", nrow(metrics), "rows from", min(metrics$date), "to", max(metrics$date), "\n")
cat("   (automatically using current month to date)\n\n")

# Example 3: Get top games - minimal parameters needed
cat("3. Getting top role-playing games (minimal parameters):\n")
top_games <- st_top_active_users(
  comparison_attribute = "absolute",
  time_range = "month", 
  measure = "MAU",
  category = "7014"  # Role Playing category
)
cat("   Found", nrow(top_games), "games using defaults:\n")
cat("   - OS: unified (worldwide)\n")
cat("   - Regions: WW (worldwide)\n") 
cat("   - Date: current month start\n")
cat("   - Limit: 20\n\n")

cat("=== Before vs After Comparison ===\n\n")

cat("BEFORE (required many parameters):\n")
cat('st_top_active_users(\n')
cat('  os = "unified",\n')
cat('  comparison_attribute = "absolute",\n')
cat('  time_range = "month",\n')
cat('  measure = "MAU",\n')
cat('  date = lubridate::floor_date(Sys.Date(), "month"),\n')
cat('  regions = "WW",\n')
cat('  category = "7014",\n')
cat('  limit = 20\n')
cat(')\n\n')

cat("AFTER (minimal parameters):\n")
cat('st_top_active_users(\n')
cat('  comparison_attribute = "absolute",\n')
cat('  time_range = "month",\n') 
cat('  measure = "MAU",\n')
cat('  category = "7014"\n')
cat(')\n\n')

cat("✅ Much cleaner and easier to use!\n")
cat("📱 Perfect for quick mobile app analytics!\n") 