# Example: Getting Unified Daily Revenue Data
# 
# This example shows how to get unified daily revenue data
# using the improved st_sales_report() with proper deduplication

library(sensortowerR)
library(dplyr)
library(ggplot2)

# Example 1: Using st_sales_report() directly (RECOMMENDED)
# ---------------------------------------------------------

# Define your app IDs
homescapes_ios <- "1195621598"
homescapes_android <- "com.playrix.homescapes"

# Set date range
end_date <- Sys.Date() - 1
start_date <- end_date - 30

cat("Method 1: Using st_sales_report() for platform-specific data\n\n")

# Method 1a: Try unified approach first (if supported)
cat("Attempting unified approach...\n")
unified_attempt <- tryCatch({
  st_sales_report(
    app_ids = c(homescapes_ios, homescapes_android),
    os = "unified",
    countries = "US",
    start_date = start_date,
    end_date = end_date,
    date_granularity = "daily"
  )
}, error = function(e) {
  cat("  Unified not supported for this endpoint, falling back to platform-specific\n")
  NULL
})

if (!is.null(unified_attempt) && nrow(unified_attempt) > 0) {
  cat(sprintf("  ✅ Unified records: %d\n", nrow(unified_attempt)))
  unified_daily <- unified_attempt %>%
    group_by(date) %>%
    summarise(
      total_revenue = sum(coalesce(total_revenue, revenue), na.rm = TRUE),
      total_downloads = sum(coalesce(total_downloads, downloads), na.rm = TRUE),
      .groups = "drop"
    )
} else {
  # Method 1b: Platform-specific fallback
  cat("Using platform-specific approach...\n")
  
  # Fetch iOS daily revenue
  ios_daily <- st_sales_report(
    app_ids = homescapes_ios,
    os = "ios",
    countries = "US",
    start_date = start_date,
    end_date = end_date,
    date_granularity = "daily"
  )
  
  cat(sprintf("  iOS records: %d\n", nrow(ios_daily)))
  
  # Fetch Android daily revenue
  android_daily <- st_sales_report(
    app_ids = homescapes_android,
    os = "android",
    countries = "US",
    start_date = start_date,
    end_date = end_date,
    date_granularity = "daily"
  )
  
  cat(sprintf("  Android records: %d\n", nrow(android_daily)))
  
  # Combine into unified daily totals
  unified_daily <- bind_rows(
    ios_daily %>% mutate(platform = "iOS"),
    android_daily %>% mutate(platform = "Android")
  ) %>%
    group_by(date) %>%
    summarise(
      total_revenue = sum(coalesce(total_revenue, revenue), na.rm = TRUE),
      total_downloads = sum(coalesce(total_downloads, downloads), na.rm = TRUE),
      platforms = paste(unique(platform), collapse = "+"),
      .groups = "drop"
    )
}


cat(sprintf("\nUnified daily records: %d\n", nrow(unified_daily)))
cat(sprintf("Total revenue: $%s\n", format(sum(unified_daily$total_revenue), big.mark = ",")))

# Calculate Gini coefficient
calculate_gini <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if (length(x) == 0) return(NA)
  x <- sort(x)
  n <- length(x)
  index <- 1:n
  gini <- (2 * sum(index * x)) / (n * sum(x)) - (n + 1) / n
  return(gini)
}

gini <- calculate_gini(unified_daily$total_revenue)
cat(sprintf("Gini coefficient: %.3f\n", gini))

# Create visualization
p <- ggplot(unified_daily, aes(x = date, y = total_revenue)) +
  geom_line(color = "#2E3192", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "#FF6B35") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Homescapes Unified Daily Revenue",
    subtitle = sprintf("iOS + Android combined | Gini: %.3f", gini),
    x = NULL,
    y = "Daily Revenue (USD)"
  ) +
  theme_minimal()

ggsave("unified_daily_revenue_example.png", p, width = 10, height = 6, dpi = 150)


# Example 2: Using the new st_metrics_v2() function
# -------------------------------------------------
if (exists("st_metrics_v2")) {
  cat("\n\nMethod 2: Using st_metrics_v2() wrapper function\n\n")
  
  metrics_v2 <- st_metrics_v2(
    unified_app_id = homescapes_ios,
    ios_app_id = homescapes_ios,
    android_app_id = homescapes_android,
    start_date = start_date,
    end_date = end_date,
    countries = "US"
  )
  
  # Create unified totals
  unified_v2 <- unify_daily_metrics(metrics_v2)
  
  cat(sprintf("Unified records from v2: %d\n", nrow(unified_v2)))
  cat(sprintf("Matches manual method: %s\n", 
              nrow(unified_v2) == nrow(unified_daily)))
}


# Example 3: Using st_top_charts with deduplication for market analysis
# ---------------------------------------------------------------------
cat("\n\nMethod 3: Using st_top_charts() with proper deduplication\n\n")

# Get top puzzle games with proper deduplication
top_games <- tryCatch({
  st_top_charts(
    measure = "revenue",
    category = 7003,  # Puzzle games
    os = "unified",
    regions = "US",
    time_range = "month",
    limit = 10,
    enrich_response = TRUE,
    deduplicate_apps = TRUE  # Ensure proper deduplication
  )
}, error = function(e) {
  cat("Error:", e$message, "\n")
  NULL
})

if (!is.null(top_games)) {
  cat(sprintf("Top games retrieved: %d\n", nrow(top_games)))
  
  # Check for unified IDs
  if ("unified_app_id" %in% names(top_games)) {
    # Verify unified IDs are preserved (24-char hex format)
    sample_ids <- head(top_games$unified_app_id, 3)
    cat("\nChecking unified ID format:\n")
    for (id in sample_ids) {
      is_hex <- grepl("^[a-f0-9]{24}$", id)
      cat(sprintf("  %s: %s\n", 
                  substr(id, 1, 8), 
                  ifelse(is_hex, "✅ Valid unified ID", "❌ Platform-specific ID")))
    }
    
    # Count unique apps after deduplication
    n_unique <- length(unique(top_games$unified_app_id))
    cat(sprintf("\nUnique apps after deduplication: %d\n", n_unique))
    
    if (n_unique < nrow(top_games)) {
      cat("⚠️  Deduplication may not be working properly\n")
    } else {
      cat("✅ Deduplication working correctly\n")
    }
  }
}

cat("\n✅ Recommendation: Try unified OS first, then fall back to platform-specific if needed\n")