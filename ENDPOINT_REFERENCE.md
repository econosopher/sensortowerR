# Sensor Tower API Endpoint Reference Card

## Quick Reference: What Works for Daily Data

| Endpoint | Daily | Weekly | Monthly | Notes |
|----------|-------|--------|---------|-------|
| `/v1/unified/sales_report_estimates` | ❌ | ❌ | ❌ | Returns empty for all granularities |
| `/v1/ios/sales_report_estimates` | ✅ | ✅ | ✅ | Full platform-specific data |
| `/v1/android/sales_report_estimates` | ✅ | ✅ | ✅ | Full platform-specific data |
| `/v1/unified/usage/active_users` | ❌ | ? | ? | Returns empty |

## Key Findings

### ❌ What Doesn't Work
- **Unified endpoints with daily data**: Always returns empty response
- **Unified app IDs**: The unified endpoint doesn't work regardless of ID format (numeric iOS or Android package)

### ✅ What Does Work
- **Platform-specific endpoints**: iOS and Android endpoints work perfectly with daily granularity
- **Multiple apps**: Can query multiple apps in one request (comma-separated IDs)
- **All date granularities**: Platform endpoints support daily, weekly, monthly, quarterly

## Recommended Approach for Daily Data

```r
# ✅ CORRECT: Use platform-specific endpoints
ios_data <- st_sales_report(
  app_ids = "1195621598",
  os = "ios",
  countries = "US",
  date_granularity = "daily"
)

android_data <- st_sales_report(
  app_ids = "com.playrix.homescapes",
  os = "android",
  countries = "US",
  date_granularity = "daily"
)

# Combine for unified view
unified <- bind_rows(ios_data, android_data) %>%
  group_by(date) %>%
  summarise(
    revenue = sum(total_revenue, na.rm = TRUE),
    downloads = sum(total_downloads, na.rm = TRUE)
  )

# ❌ INCORRECT: This will return empty
metrics <- st_metrics(
  unified_app_id = "1195621598",
  start_date = start_date,
  end_date = end_date
)
```

## Test Scripts Available

1. **`tests/quick_endpoint_test.R`** - Basic functionality test (30 seconds)
2. **`tests/parallel_endpoint_test.R`** - Comprehensive parallel testing
3. **`tests/test_endpoints.R`** - Full test suite with visualizations

## Running Tests

```bash
# Quick test
Rscript tests/quick_endpoint_test.R

# Parallel test (faster for many endpoints)
Rscript tests/parallel_endpoint_test.R

# Full test suite
Rscript tests/test_endpoints.R
```

## Important Notes

1. The Sensor Tower unified API appears to be designed for aggregated metrics, not granular daily data
2. Always use platform-specific endpoints for daily revenue/download data
3. You need both iOS and Android app IDs to get complete unified data
4. The API automatically segments long date ranges to prevent timeouts

## Updated Functions

- `st_metrics()` - Now warns about daily data limitations
- `st_metrics_v2()` - New function that automatically uses platform endpoints
- `st_sales_report()` - The reliable workhorse for daily data