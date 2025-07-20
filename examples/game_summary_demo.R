# Demo: Game Market Summary Analysis
# This script demonstrates the new st_game_summary() function

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = c("devtools", "dplyr", "gt", "ggplot2", "tidyr", "scales", "webshot2"))

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

# --- Create Visualizations ---

# Create output directory
output_dir <- "inst/images"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 1. Multi-Country Revenue Comparison
cat("\n📊 Creating Revenue Comparison Visualization...\n")
revenue_comparison <- multi_country %>%
  select(Date, `Country Code`, `iOS Revenue`) %>%
  ggplot(aes(x = Date, y = `iOS Revenue`, color = `Country Code`)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_color_brewer(palette = "Dark2", name = "Country") +
  labs(
    title = "iOS Game Revenue by Country",
    subtitle = "Daily revenue trends across major markets",
    x = "Date",
    y = "Revenue (USD Millions)",
    caption = "Data source: Sensor Tower API"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(output_dir, "game_market_revenue_comparison.png"),
  plot = revenue_comparison,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# 2. Downloads vs Revenue Analysis
cat("📊 Creating Downloads vs Revenue Analysis...\n")
downloads_revenue_plot <- multi_country %>%
  group_by(`Country Code`) %>%
  summarise(
    Total_Revenue = sum(`iOS Revenue`, na.rm = TRUE),
    Total_Downloads = sum(`iOS Downloads`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Revenue_Millions = Total_Revenue / 1000000,
    Downloads_Millions = Total_Downloads / 1000000,
    RPD = Total_Revenue / Total_Downloads
  ) %>%
  ggplot(aes(x = Downloads_Millions, y = Revenue_Millions)) +
  geom_point(aes(size = RPD, color = `Country Code`), alpha = 0.7) +
  geom_text(aes(label = `Country Code`), vjust = -1.5, size = 4) +
  scale_x_continuous(labels = scales::comma_format(suffix = "M")) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M")) +
  scale_size_continuous(range = c(5, 15), name = "RPD (USD)") +
  scale_color_brewer(palette = "Set1", guide = "none") +
  labs(
    title = "Game Market Performance by Country",
    subtitle = "Total downloads vs revenue (bubble size = revenue per download)",
    x = "Total Downloads (Millions)",
    y = "Total Revenue (USD Millions)",
    caption = "Data source: Sensor Tower API"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(output_dir, "game_market_performance.png"),
  plot = downloads_revenue_plot,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 3. Save the GT table as PNG
cat("📊 Saving summary table as PNG...\n")
formatted_table %>%
  gtsave(
    filename = file.path(output_dir, "game_market_summary_table.png"),
    vwidth = 1000,
    vheight = 600
  )

cat("\n✅ Visualizations created successfully!\n")
cat("Files saved to:", output_dir, "\n")
cat("- game_market_revenue_comparison.png\n")
cat("- game_market_performance.png\n")
cat("- game_market_summary_table.png\n") 