# sensortowerR Platform Information Update

## Changes Made

### Updated Functions
The following functions now return platform information in their results:

1. **st_sales_report()**
   - Added `platform` column (ios/android)

2. **st_top_charts()**
   - Added `platform` column (ios/android/unified)

3. **st_metrics()**
   - When `combine_platforms = TRUE`: Sets platform as "unified" 
   - When `combine_platforms = FALSE`: Preserves individual platform info

4. **st_ytd_metrics()**
   - Intelligently determines platform based on input parameters

5. **st_category_rankings()**
   - Added platform info based on `os` parameter

### Example Usage

```r
# iOS-only query
ios_data <- st_sales_report(app_ids = "123", os = "ios", ...)
# Results include: platform = "ios"

# Unified query
unified_data <- st_metrics(
  ios_app_id = "123",
  android_app_id = "com.example.app",
  combine_platforms = TRUE,
  ...
)
# Results include: platform = "unified"
```

### Benefits
- Prevents iOS-only data being mistaken for global revenue
- No breaking changes - existing code continues to work
- Easy to filter results by platform
- Clear indication of data coverage in every result