# Demo: Game Market Summary Analysis
# This script demonstrates the new st_game_summary() function

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = c("devtools", "dplyr", "gt", "ggplot2"))

# Load the development version
devtools::load_all()

cat("=== 🎮 GAME MARKET SUMMARY DEMO ===\n")

# Example 1: Basic iOS game market summary
cat("\n📊 Example 1: iOS Games Market (Last 3 days)\n")
ios_games <- st_game_summary(
  categories = 7001,          # Game category
  os = "ios",                 # iOS platform
  countries = "US",           # US market
  date_granularity = "daily", # Daily breakdown
  start_date = Sys.Date() - 3,
  end_date = Sys.Date() - 1
)

cat("Got", nrow(ios_games), "data points\n")
cat("Columns:", paste(names(ios_games), collapse = ", "), "\n")

# Show summary by date
daily_summary <- ios_games %>%
  group_by(Date) %>%
  summarise(
    Total_iOS_Downloads = sum(`iOS Downloads`, na.rm = TRUE),
    Total_iOS_Revenue = sum(`iOS Revenue`, na.rm = TRUE),
    .groups = "drop"
  )

print(daily_summary)

# Example 2: Multi-country comparison
cat("\n🌍 Example 2: Multi-Country Game Market\n")
multi_country <- st_game_summary(
  categories = 7001,
  os = "ios",
  countries = c("US", "GB", "DE", "JP"),
  date_granularity = "daily",
  start_date = Sys.Date() - 2,
  end_date = Sys.Date() - 1
)

# Top countries by total revenue
country_revenue <- multi_country %>%
  group_by(`Country Code`) %>%
  summarise(
    Total_Revenue = sum(`iOS Revenue`, na.rm = TRUE),
    Total_Downloads = sum(`iOS Downloads`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue))

cat("Top countries by revenue:\n")
print(country_revenue)

# Example 3: Weekly aggregation
cat("\n📅 Example 3: Weekly Market Trends\n")
weekly_data <- st_game_summary(
  categories = 7001,
  os = "unified",
  countries = "WW",
  date_granularity = "weekly", 
  start_date = Sys.Date() - 21,  # 3 weeks
  end_date = Sys.Date() - 1
)

cat("Weekly data points:", nrow(weekly_data), "\n")
if (nrow(weekly_data) > 0) {
  cat("Sample weekly data:\n")
  print(head(weekly_data, 3))
}

# Create a summary table
cat("\n📋 Summary Table\n")
summary_table <- ios_games %>%
  group_by(`Country Code`, Date) %>%
  summarise(
    iOS_Downloads = sum(`iOS Downloads`, na.rm = TRUE),
    iOS_Revenue = sum(`iOS Revenue`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(iOS_Revenue)) %>%
  head(10)

formatted_table <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Top iOS Game Market Performance",
    subtitle = "Daily downloads and revenue by country (iPhone + iPad combined)"
  ) %>%
  fmt_currency(
    columns = iOS_Revenue,
    currency = "USD",
    decimals = 0
  ) %>%
  fmt_number(
    columns = iOS_Downloads,
    decimals = 0
  ) %>%
  fmt_date(
    columns = Date,
    date_style = 6
  )

print(formatted_table)

cat("\n🎯 Key Features Demonstrated:\n")
cat("✅ Multiple platforms (iOS, Android, Unified)\n")
cat("✅ Automatic iPhone + iPad data combination\n")
cat("✅ Flexible date ranges and granularities\n")
cat("✅ Multi-country analysis\n")
cat("✅ Automatic field name mapping\n")
cat("✅ Clean, analysis-ready data\n")
cat("✅ Integration with tidyverse and gt\n") 