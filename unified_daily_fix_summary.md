# Summary of Changes to Handle Unified Daily Revenue Issue

## Problem
The `st_metrics()` function returns 0 rows when requesting daily data because the unified sales_report_estimates endpoint doesn't provide daily granular data.

## Changes Made

### 1. Updated st_metrics.R
- Added a warning message when daily data is requested
- Suggests using `st_sales_report()` or the new `st_metrics_v2()`

### 2. Created st_metrics_v2.R
- New function that automatically fetches platform-specific data
- Accepts both unified and platform-specific app IDs
- Combines iOS and Android data into unified totals
- Includes helper function `unify_daily_metrics()`

### 3. Updated README.md
- Added note about st_metrics() limitation
- Provided example code showing how to use st_sales_report()
- Documented the workaround for getting unified daily data

### 4. Updated NAMESPACE
- Added exports for `st_metrics_v2` and `unify_daily_metrics`

### 5. Created Example
- `unified_daily_revenue_example.R` demonstrates all methods
- Shows the issue with original st_metrics()
- Provides working solutions

## Usage

### Option 1: Use st_sales_report() directly (recommended)
```r
# Fetch platform-specific data
ios_data <- st_sales_report(app_ids = "ios_id", os = "ios", date_granularity = "daily")
android_data <- st_sales_report(app_ids = "android_id", os = "android", date_granularity = "daily")

# Combine
unified <- bind_rows(ios_data, android_data) %>%
  group_by(date) %>%
  summarise(total_revenue = sum(total_revenue, na.rm = TRUE))
```

### Option 2: Use new st_metrics_v2()
```r
metrics <- st_metrics_v2(
  unified_app_id = "1195621598",
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes"
)

unified <- unify_daily_metrics(metrics)
```

## Next Steps
1. Test the changes thoroughly
2. Update package version
3. Document in NEWS.md
4. Consider deprecating st_metrics() in favor of st_metrics_v2()