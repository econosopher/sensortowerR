# Migration Guide: st_metrics Update

## What Changed

The `st_metrics()` function has been updated to automatically handle the limitation where Sensor Tower's unified endpoint doesn't return daily data.

## New Behavior

1. **Automatic platform detection**: The function now detects if your app_id is iOS (numeric) or Android (package name)
2. **Smart endpoint selection**: For daily data, it automatically uses platform-specific endpoints
3. **Unified data by default**: iOS and Android data are combined unless you set `combine_platforms = FALSE`

## Migration Examples

### Old way (often returned empty data):
```r
# This would return 0 rows for daily data
metrics <- st_metrics(
  unified_app_id = "1195621598",
  start_date = start_date,
  end_date = end_date
)
```

### New way (works automatically):
```r
# Option 1: Simple - let the function figure it out
metrics <- st_metrics(
  app_id = "1195621598",  # Note: renamed from unified_app_id
  start_date = start_date,
  end_date = end_date
)

# Option 2: Explicit - provide both platforms for complete data
metrics <- st_metrics(
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes",
  start_date = start_date,
  end_date = end_date
)
```

## Parameter Changes

| Old Parameter | New Parameter | Notes |
|--------------|---------------|-------|
| `unified_app_id` | `app_id` | Still works but shows deprecation warning |
| N/A | `ios_app_id` | New - explicitly specify iOS app |
| N/A | `android_app_id` | New - explicitly specify Android app |
| N/A | `combine_platforms` | New - set FALSE to keep platforms separate |
| N/A | `auto_platform_fetch` | New - set FALSE to force unified endpoint |
| N/A | `date_granularity` | New - supports daily, weekly, monthly, quarterly |
| N/A | `countries` | New - specify country codes |
| N/A | `verbose` | New - control progress messages |

## Breaking Changes

None - the function is backward compatible. Old code will continue to work but may show deprecation warnings.

## Benefits

1. **It just works**: No more empty results for daily data
2. **True unified data**: Automatically combines iOS and Android
3. **Flexible**: Can still access platform-specific data if needed
4. **Smart defaults**: Detects platform from app ID format

## Troubleshooting

If you're still getting empty results:
1. Check that your app IDs are correct
2. Verify the date range has data
3. Ensure your API token is valid
4. Try setting `verbose = TRUE` to see what's happening

## Code to Update

If you have code like this:
```r
# Manual workaround for daily data
ios_data <- st_sales_report(app_ids = ios_id, os = "ios", ...)
android_data <- st_sales_report(app_ids = android_id, os = "android", ...)
unified <- bind_rows(ios_data, android_data) %>% 
  group_by(date) %>% 
  summarise(...)
```

You can now simply use:
```r
metrics <- st_metrics(
  ios_app_id = ios_id,
  android_app_id = android_id,
  ...
)
```