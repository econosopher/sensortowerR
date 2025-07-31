# Sensor Tower API Endpoint Reference Card

## Custom Field Filter Support

| Endpoint | Supports custom_fields_filter_id | Notes |
|----------|----------------------------------|-------|
| `/v1/unified/sales_report_estimates` | ✅ | Full support for custom filters |
| `/v1/unified/ranking` | ✅ | Category rankings with custom filters |
| `/v1/unified/sales_report_estimates_comparison_attributes` | ✅ | Top charts with custom filters |
| `/v1/unified/active_users` | ❌ | Does not support custom filters |
| `/v1/unified/top_publishers` | ❌ | Does not support custom filters |
| `/v1/unified/engagement_report` | ❌ | Does not support custom filters |
| `/v1/unified/ad_spend_estimates` | ❌ | Does not support custom filters |
| `/v1/unified/games/breakdown` | ❌ | Does not support custom filters |

### Custom Filter Usage

Custom field filters allow you to use predefined filters from the Sensor Tower web interface:

```r
# Example: Use a custom filter for category rankings
rankings <- st_category_rankings(
  os = "unified",
  custom_fields_filter_id = "67890abcdef1234567890abc",
  custom_tags_mode = "include_unified_apps",
  country = "US",
  date = "2024-01-01"
)

# Example: Sales report with custom filter
sales <- st_sales_report(
  custom_fields_filter_id = "67890abcdef1234567890abc",
  countries = "US",
  date_granularity = "monthly",
  start_date = "2024-01-01",
  end_date = "2024-03-31"
)
```

### Important Notes for Custom Filters

1. **custom_fields_filter_id**: 24-character hexadecimal string from Sensor Tower web interface
2. **custom_tags_mode**: Required for unified OS, controls filter behavior:
   - `"include"` - Include apps matching the filter
   - `"exclude"` - Exclude apps matching the filter
   - `"include_unified_apps"` - Include unified apps matching the filter
3. When using custom filters, some normally required parameters become optional (e.g., category in st_category_rankings)

## Quick Reference: What Works for Daily Data

| Function | Daily | Weekly | Monthly | Notes |
|----------|-------|--------|---------|-------|
| `st_metrics(os='unified')` | ✅ | ✅ | ✅ | Works perfectly! Combines iOS & Android data |
| `st_sales_report(os='unified')` | ❌ | ❌ | ❌ | Not implemented for unified OS |
| `st_sales_report(os='ios')` | ✅ | ✅ | ✅ | Full platform-specific data |
| `st_sales_report(os='android')` | ✅ | ✅ | ✅ | Full platform-specific data |
| `st_batch_metrics(os='unified')` | ❌ | ❌ | ❌ | Use platform-specific OS instead |

## Key Findings

### ❌ What Doesn't Work

- **st_sales_report() with os='unified'**: Not implemented
- **st_batch_metrics() with os='unified'**: Not supported
- **Unified app lookup**: The API returns HTTP 422 errors when trying to resolve unified IDs

### ✅ What Does Work

- **st_metrics() with os='unified'**: Works perfectly for daily, weekly, and monthly data!
- **Platform-specific endpoints**: iOS and Android endpoints work with all granularities
- **Unified data retrieval**: st_metrics() automatically combines iOS and Android data when both IDs are provided
- **All date granularities**: Daily, weekly, monthly, and quarterly are supported

## Recommended Approach for Daily Data

```r
# ✅ CORRECT: Use st_metrics() for unified data
unified_data <- st_metrics(
  os = "unified",
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes",
  countries = "US",
  date_granularity = "daily",
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)
# This automatically combines iOS and Android data!

# ✅ CORRECT: Use platform-specific endpoints with matching app IDs
# iOS example - numeric app ID required
ios_data <- st_sales_report(
  app_ids = "1195621598",  # iOS numeric ID
  os = "ios",
  countries = "US",
  date_granularity = "daily",
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)

# Android example - package name required  
android_data <- st_sales_report(
  app_ids = "com.playrix.homescapes",  # Android package name
  os = "android",
  countries = "US",
  date_granularity = "daily",
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)

# ❌ INCORRECT: Mismatched app ID formats
# This will throw an error:
# st_sales_report(
#   app_ids = "1195621598",  # iOS ID
#   os = "android",           # But requesting Android!
#   countries = "US",
#   date_granularity = "daily"
# )
# Error: iOS app ID provided but Android ID required

# ❌ INCORRECT: Unified OS not supported
# st_sales_report(
#   app_ids = "5ba4585f539ce75b97db6bcb",  # Unified ID
#   os = "unified",
#   countries = "US",
#   date_granularity = "daily"
# )
# Error: st_sales_report does not support os='unified'
```

## App ID Format Guide

### Understanding App ID Types

| Platform | Format | Example | Description |
|----------|--------|---------|-------------|
| iOS | Numeric string | `"1195621598"` | Apple App Store ID |
| Android | Package name | `"com.playrix.homescapes"` | Google Play package identifier |
| Unified | 24-char hex | `"5ba4585f539ce75b97db6bcb"` | Sensor Tower's internal unified ID |

### App ID Validation Rules

1. **st_sales_report()** requires platform-specific IDs:
   - With `os="ios"`: Must provide iOS numeric IDs
   - With `os="android"`: Must provide Android package names
   - Does NOT support `os="unified"`

2. **st_metrics()** with `os="unified"`:
   - Requires BOTH `ios_app_id` and `android_app_id` parameters
   - Automatically combines data from both platforms

3. **Mismatched IDs**: The package now validates app ID formats:
   - Providing an iOS ID with `os="android"` will error
   - Providing an Android ID with `os="ios"` will error
   - Clear error messages guide you to the correct format

4. **Unified ID Lookup**: Use `st_app_lookup()` to convert:

   ```r
   # Convert unified ID to platform-specific IDs
   app_info <- st_app_lookup("5ba4585f539ce75b97db6bcb")
   # Returns: list(ios_app_id = "1195621598", 
   #               android_app_id = "com.playrix.homescapes",
   #               app_name = "Homescapes",
   #               publisher_name = "Playrix")
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

1. **UPDATE**: The unified API DOES work for daily/weekly/monthly data when using st_metrics()!
2. For unified data, use st_metrics() with os='unified' and provide both iOS and Android IDs
3. st_sales_report() does not support os='unified' - use platform-specific calls instead
4. The API automatically segments long date ranges to prevent timeouts
5. Active user metrics (DAU/WAU/MAU) may require platform-specific calls

## Updated Functions

- `st_metrics()` - **Works perfectly with unified data!** Use os='unified' with both app IDs
- `st_sales_report()` - Platform-specific only (ios/android), now supports custom_fields_filter_id
- `st_category_rankings()` - Now supports custom field filters as alternative to category
- `st_top_charts()` - Supports custom field filters through the comparison attributes endpoint
- `st_batch_metrics()` - Use with platform-specific OS, not unified

## Version History

### v0.7.5 (2025-01-30)

- Added custom field filter support to st_category_rankings and st_sales_report
- Fixed st_app_lookup test to use correct parameter name (unified_id)
- Consolidated API response analysis tools
- Updated documentation for custom filter usage
- **IMPORTANT**: Corrected misinformation about unified endpoints - they DO work for daily/weekly/monthly data!
