# Demo: API Call Optimization Features
# This script demonstrates the batching and caching optimizations

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = c("devtools", "dplyr", "gt"))

# Load the development version
devtools::load_all()

cat("=== 🚀 API OPTIMIZATION DEMO ===\n")

# Clear cache to start fresh
st_clear_app_cache()

# Example 1: Demonstrate caching with multiple calls
cat("\n📊 Example 1: App Name Caching Optimization\n")

# First call - will make API calls and cache results
cat("First call (will make API calls):\n")
system.time({
  result1 <- st_top_charts(
    measure = "MAU", 
    category = 7014,  # Role Playing
    limit = 5
  )
})

cat("\nSecond call with same category (should use cache):\n")
system.time({
  result2 <- st_top_charts(
    measure = "MAU", 
    category = 7014,  # Same category
    limit = 5,
    date = Sys.Date() - 1  # Different date but same apps likely
  )
})

cat("Cache status: Apps in cache:", length(ls(envir = sensortowerR:::.app_name_cache)), "\n")

# Example 2: Batch processing multiple categories efficiently
cat("\n🔄 Example 2: Efficient Multi-Category Analysis\n")

# Instead of multiple separate calls, we can combine where possible
categories_to_analyze <- c(7014, 7001, 7002)  # Role Playing, Action, Adventure

# Efficient approach: Use smaller limits and combine results
cat("Fetching top apps from multiple categories efficiently...\n")

combined_results <- list()
for (i in seq_along(categories_to_analyze)) {
  cat(sprintf("Fetching category %d (%d/%d)...\n", 
              categories_to_analyze[i], i, length(categories_to_analyze)))
  
  combined_results[[i]] <- st_top_charts(
    measure = "MAU",
    category = categories_to_analyze[i],
    limit = 3  # Smaller limit for demo
  ) %>%
    mutate(category_id = categories_to_analyze[i])
}

# Combine all results
all_results <- bind_rows(combined_results)

cat("\n📋 Combined Results Summary\n")
summary_table <- all_results %>%
  group_by(category_id) %>%
  summarise(
    apps_count = n(),
    avg_mau = mean(mau_month_ww, na.rm = TRUE),
    top_app = first(unified_app_name),
    .groups = "drop"
  )

print(summary_table)

# Example 3: Game Summary Batching
cat("\n🎮 Example 3: Game Summary Date Range Optimization\n")

# Instead of multiple daily calls, use larger date ranges
cat("Fetching weekly data (more efficient than daily):\n")
weekly_summary <- st_game_summary(
  categories = 7001,
  os = "ios", 
  countries = "US",
  date_granularity = "weekly",  # More efficient than daily
  start_date = Sys.Date() - 21,
  end_date = Sys.Date() - 1
)

cat("Weekly data points:", nrow(weekly_summary), "\n")

# Example 4: Cache performance comparison
cat("\n⚡ Example 4: Cache Performance Demonstration\n")

# Clear cache and time fresh lookup
st_clear_app_cache()
cat("With empty cache:\n")
time_no_cache <- system.time({
  fresh_result <- st_top_charts(measure = "revenue", category = 6000, limit = 3)
})

cat("With populated cache (same request):\n") 
time_with_cache <- system.time({
  cached_result <- st_top_charts(measure = "revenue", category = 6000, limit = 3)
})

cat(sprintf("Speed improvement: %.1fx faster with cache\n", 
            time_no_cache[["elapsed"]] / time_with_cache[["elapsed"]]))

cat("\n🏆 Optimization Features Demonstrated:\n")
cat("✅ App name caching (avoids redundant API calls)\n")
cat("✅ Batched lookups (reduced API call frequency)\n") 
cat("✅ Efficient date range queries\n")
cat("✅ Smart cache utilization across multiple calls\n")
cat("✅ Progress reporting and success rate tracking\n")
cat("✅ Manual cache management (st_clear_app_cache)\n") 