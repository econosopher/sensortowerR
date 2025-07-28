# sensortowerR v0.2.3+ Revenue Standardization Example
# This script demonstrates the automatic revenue standardization feature

library(tidyverse)
library(sensortowerR)
library(scales)

cat("sensortowerR Revenue Standardization Example\n")
cat("============================================\n\n")

# Check package version
pkg_version <- packageVersion("sensortowerR")
cat(sprintf("Current sensortowerR version: %s\n", pkg_version))

if (pkg_version < "0.2.3") {
  cat("\nWARNING: Please update to sensortowerR v0.2.3+ for automatic revenue standardization\n")
  cat("Install the latest version with: devtools::install_github('econosopher/sensortowerR')\n\n")
}

# Example 1: st_top_charts() with standardized revenue
cat("\n1. Fetching top games with standardized revenue units\n")
cat("=====================================================\n")

top_games <- st_top_charts(
  measure = "revenue",
  category = 6000,  # Games
  os = "ios",
  regions = "US",
  time_range = "month",
  limit = 5
)

# Display the standardized revenue column
cat("\nStandardized revenue values (in base currency units):\n")
if ("revenue" %in% names(top_games)) {
  display_data <- top_games %>%
    select(unified_app_name, revenue, revenue_absolute) %>%
    mutate(
      revenue_formatted = dollar(revenue, accuracy = 0.1, scale = 1e-6, suffix = "M"),
      cents_formatted = comma(revenue_absolute, suffix = "¢"),
      conversion_check = round(revenue_absolute / revenue, 0)
    )
  
  print(display_data, n = 5)
  
  cat("\nRevenue unit attributes:\n")
  cat(sprintf("- revenue unit: %s\n", attr(top_games$revenue, "unit") %||% "base_currency"))
  cat(sprintf("- revenue_absolute unit: %s\n", attr(top_games$revenue_absolute, "unit") %||% "cents"))
} else {
  cat("Note: 'revenue' column not found. Please update to sensortowerR v0.2.3+\n")
}

# Example 2: Demonstrating unit consistency across functions
cat("\n\n2. Unit Consistency Across Functions\n")
cat("====================================\n")

# Get a single app for detailed analysis
if (nrow(top_games) > 0) {
  sample_app <- top_games$app_id[1]
  sample_name <- top_games$unified_app_name[1]
  
  cat(sprintf("\nAnalyzing: %s (App ID: %s)\n", sample_name, sample_app))
  
  # Fetch sales report data
  cat("\nFetching sales report data...\n")
  sales_data <- st_sales_report(
    app_ids = sample_app,
    os = "ios",
    countries = "US",
    start_date = Sys.Date() - 30,
    end_date = Sys.Date() - 1,
    date_granularity = "monthly"
  )
  
  if (!is.null(sales_data) && nrow(sales_data) > 0) {
    cat("\nRevenue comparison:\n")
    cat(sprintf("- Top charts revenue (30d): %s\n", 
                dollar(top_games$revenue[1], accuracy = 0.1, scale = 1e-6, suffix = "M")))
    cat(sprintf("- Sales report total: %s\n", 
                dollar(sales_data$total_revenue[1], accuracy = 0.1, scale = 1e-6, suffix = "M")))
    cat("\nNote: Values may differ due to timing and data sources\n")
  }
}

# Example 3: Working with the standardized data
cat("\n\n3. Analysis with Standardized Revenue\n")
cat("=====================================\n")

# Category comparison
cat("\nComparing game subcategories (standardized revenue):\n")

categories <- list(
  "Puzzle" = 7003,
  "Action" = 7001,
  "RPG" = 7014
)

category_data <- map_dfr(names(categories), function(cat_name) {
  cat_data <- st_top_charts(
    measure = "revenue",
    category = categories[[cat_name]],
    os = "ios",
    regions = "US",
    time_range = "month",
    limit = 10
  )
  
  if (!is.null(cat_data) && nrow(cat_data) > 0 && "revenue" %in% names(cat_data)) {
    tibble(
      category = cat_name,
      total_revenue = sum(cat_data$revenue, na.rm = TRUE),
      avg_revenue = mean(cat_data$revenue, na.rm = TRUE),
      top_game_revenue = max(cat_data$revenue, na.rm = TRUE),
      n_games = nrow(cat_data)
    )
  } else {
    NULL
  }
})

if (nrow(category_data) > 0) {
  category_summary <- category_data %>%
    mutate(
      total_formatted = dollar(total_revenue, accuracy = 0.1, scale = 1e-6, suffix = "M"),
      avg_formatted = dollar(avg_revenue, accuracy = 0.1, scale = 1e-6, suffix = "M"),
      top_formatted = dollar(top_game_revenue, accuracy = 0.1, scale = 1e-6, suffix = "M")
    ) %>%
    select(category, n_games, total_formatted, avg_formatted, top_formatted)
  
  print(category_summary)
}

# Example 4: Best practices
cat("\n\n4. Best Practices\n")
cat("==================\n")

cat("
1. Always use the standardized 'revenue' column for analysis
2. The original 'revenue_absolute' (cents) is preserved for reference
3. Check for the 'revenue' column to ensure you're using v0.2.3+
4. Revenue units are consistent across all sensortowerR functions:
   - st_top_charts(): 'revenue' column
   - st_sales_report(): 'total_revenue', 'iphone_revenue', etc.
   - st_top_publishers(): 'revenue_usd' column

5. Example pattern for backward compatibility:
")

cat('
# Use standardized revenue if available, fallback to manual conversion
revenue_millions <- if ("revenue" %in% names(data)) {
  data$revenue / 1e6
} else {
  data$revenue_absolute / 100 / 1e6  # Convert cents to millions
}
')

cat("\n\nFor more information, see: inst/docs/revenue_units_guide.md\n")