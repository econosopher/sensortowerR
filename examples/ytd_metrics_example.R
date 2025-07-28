# Year-to-Date Metrics Example
# This script demonstrates the st_ytd_metrics function

library(sensortowerR)
library(dplyr)
library(ggplot2)

# Example 1: Simple YTD metrics for current year
# ----------------------------------------------
# By default, fetches YTD through last completed week (ending Saturday)
current_ytd <- st_ytd_metrics(
  unified_app_id = "553834731",  # Candy Crush
  verbose = TRUE
)

# View the tidy data format
print(current_ytd)

# Example 2: Multiple years comparison
# ------------------------------------
multi_year <- st_ytd_metrics(
  unified_app_id = "1195621598",  # Homescapes
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes",
  years = c(2023, 2024, 2025),
  cache_dir = ".cache/ytd"
)

# Summarize by year
yearly_summary <- multi_year %>%
  group_by(year, metric) %>%
  summarise(total_value = sum(value, na.rm = TRUE)) %>%
  ungroup()

print(yearly_summary)

# Example 3: Custom period (February only)
# ----------------------------------------
feb_metrics <- st_ytd_metrics(
  unified_app_id = "1053012308",  # MONOPOLY GO!
  years = c(2023, 2024, 2025),
  period_start = "02-01",
  period_end = "02-28",  # Automatically handles Feb 29 in 2024
  verbose = TRUE
)

# Example 4: Multiple apps comparison
# -----------------------------------
# NEW: Process multiple apps in a single call!
app_ids <- c("553834731", "1195621598", "1053012308")
app_names <- c("Candy Crush", "Homescapes", "MONOPOLY GO!")

all_app_metrics <- st_ytd_metrics(
  unified_app_id = app_ids,
  years = c(2024, 2025),
  metrics = c("revenue", "downloads"),
  verbose = TRUE
)

# Add app names for visualization
all_app_metrics <- all_app_metrics %>%
  left_join(
    tibble(entity_id = app_ids, app_name = app_names),
    by = "entity_id"
  )

# Example 5: Visualizing YTD trends
# ---------------------------------
# Filter for revenue in US
revenue_data <- multi_year %>%
  filter(metric == "revenue", country == "US")

# Create year-over-year comparison plot
ggplot(revenue_data, aes(x = factor(year), y = value / 1e6)) +
  geom_col(fill = "#2E86AB") +
  geom_text(aes(label = sprintf("$%.1fM", value / 1e6)), 
            vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "Year-to-Date Revenue Comparison",
    subtitle = sprintf("Through %s each year", revenue_data$date_end[1]),
    x = "Year",
    y = "Revenue (USD Millions)"
  ) +
  theme(plot.title.position = "panel",
        plot.subtitle.position = "panel")

# Example 6: Calculate year-over-year growth
# ------------------------------------------
# Pivot to wide format for calculations
growth_analysis <- multi_year %>%
  filter(country == "US") %>%
  select(year, metric, value) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(
    yoy_2024_growth = (`2024` - `2023`) / `2023` * 100,
    yoy_2025_growth = (`2025` - `2024`) / `2024` * 100
  )

print(growth_analysis)

# Example 7: Fiscal year analysis
# -------------------------------
# Fiscal year starting April 1
fiscal_ytd <- st_ytd_metrics(
  unified_app_id = "553834731",
  years = c(2023, 2024, 2025),
  period_start = "04-01",
  period_end = format(Sys.Date() - 1, "%m-%d"),  # Through yesterday
  verbose = TRUE
)

# Example 8: Custom metrics (when available)
# ------------------------------------------
# As new metrics become available in the API, you can request them:
# custom_metrics <- st_ytd_metrics(
#   unified_app_id = "553834731",
#   years = 2025,
#   metrics = c("revenue", "downloads", "dau", "mau", "retention_d1")
# )

# Example 9: Publisher metrics (when implemented)
# -----------------------------------------------
# publisher_metrics <- st_ytd_metrics(
#   publisher_id = c("pub123", "pub456"),  # Multiple publishers
#   years = c(2024, 2025),
#   metrics = c("revenue", "downloads")
# )

# Example 10: Working with cached data
# -----------------------------------
# First call fetches from API
cached_call1 <- st_ytd_metrics(
  unified_app_id = "553834731",
  years = 2025,
  cache_dir = ".cache/ytd",
  verbose = TRUE
)

# Second call uses cache (within 24 hours)
cached_call2 <- st_ytd_metrics(
  unified_app_id = "553834731",
  years = 2025,
  cache_dir = ".cache/ytd",
  verbose = TRUE
)

# Example 11: Handling partial years
# ----------------------------------
# For the current year, automatically adjusts to available data
current_year_partial <- st_ytd_metrics(
  unified_app_id = "553834731",
  years = as.integer(format(Sys.Date(), "%Y")),
  period_start = "01-01",
  period_end = "12-31",  # Will adjust to actual available data
  verbose = TRUE
)