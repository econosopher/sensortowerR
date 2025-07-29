# Quick API test - uses package's default token handling
library(sensortowerR)

# Test 1: Basic st_metrics call
cat("Testing st_metrics...\n")
result1 <- st_metrics(
  os = "ios",
  ios_app_id = "553834731",  # Candy Crush
  countries = "US",
  date_granularity = "monthly",
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)

cat("Retrieved", nrow(result1), "rows\n")
cat("Revenue:", sum(result1$revenue), "\n")
cat("Downloads:", sum(result1$downloads), "\n\n")

# Test 2: st_top_charts
cat("Testing st_top_charts...\n")
result2 <- st_top_charts(
  os = "ios",
  comparison_attribute = "absolute",
  time_range = "month",
  measure = "revenue",
  date = "2024-01-01",
  category = 6014,
  regions = "US",
  limit = 5
)

cat("Retrieved top", nrow(result2), "apps\n")
print(head(result2[, 1:5]))  # Show first 5 columns

# Test 3: st_yoy_metrics
cat("\nTesting st_yoy_metrics...\n")
result3 <- st_yoy_metrics(
  os = "ios",
  ios_app_id = "553834731",
  years = c(2023, 2024),
  period_start = "01-01",
  period_end = "01-31",
  countries = "US",
  metrics = "revenue"
)

cat("Retrieved", nrow(result3), "rows for years:", paste(unique(result3$year), collapse=", "), "\n")
if (any(!is.na(result3$yoy_change))) {
  yoy_data <- result3[!is.na(result3$yoy_change), c("year", "metric", "value", "yoy_change")]
  print(yoy_data)
}