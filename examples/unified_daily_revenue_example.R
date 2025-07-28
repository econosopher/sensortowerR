# Example: Getting Unified Daily Revenue Data
# 
# This example shows how to work around the issue where st_metrics()
# returns no data for daily granularity with unified app IDs

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

# Fetch iOS daily revenue
ios_daily <- st_sales_report(
  app_ids = homescapes_ios,
  os = "ios",
  countries = "US",
  start_date = start_date,
  end_date = end_date,
  date_granularity = "daily"
)

cat(sprintf("iOS records: %d\n", nrow(ios_daily)))

# Fetch Android daily revenue
android_daily <- st_sales_report(
  app_ids = homescapes_android,
  os = "android",
  countries = "US",
  start_date = start_date,
  end_date = end_date,
  date_granularity = "daily"
)

cat(sprintf("Android records: %d\n", nrow(android_daily)))

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


# Example 3: Demonstrating the issue with original st_metrics()
# -------------------------------------------------------------
cat("\n\nMethod 3: Original st_metrics() - demonstrating the issue\n\n")

tryCatch({
  metrics_original <- st_metrics(
    unified_app_id = homescapes_ios,
    start_date = start_date,
    end_date = end_date
  )
  
  cat(sprintf("Original st_metrics records: %d\n", nrow(metrics_original)))
  
  if (nrow(metrics_original) == 0) {
    cat("❌ As expected, st_metrics() returned no data for daily granularity\n")
    cat("   This is why we need to use st_sales_report() instead\n")
  }
}, error = function(e) {
  cat("Error:", e$message, "\n")
})

cat("\n✅ Recommendation: Use st_sales_report() with platform-specific IDs for daily data\n")