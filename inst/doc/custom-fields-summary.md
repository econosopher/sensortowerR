# Custom Fields Filters - Feature Summary

## Overview
Custom Fields Filters enable advanced app segmentation in Sensor Tower using 400+ attributes including genres, monetization models, SDKs, retention metrics, and more.

## Key Functions

### Core API Functions
- `st_custom_fields_filter()` - Create custom filter from criteria
- `st_custom_fields_filter_by_id()` - Get filter details by ID
- `st_custom_fields_values()` - List all available custom fields
- `st_app_tag()` - Get apps matching a custom filter

### Workflow Helpers
- `st_create_simple_filter()` - Simplified filter creation
- `st_discover_fields()` - Search and explore available fields
- `st_get_filtered_apps()` - Complete workflow from filter to data
- `st_analyze_filter()` - Analyze apps matching a filter

### Utility Functions
- `st_filter_by_genre()` - Filter by game genres/sub-genres
- `st_filter_by_monetization()` - Filter by monetization model
- `st_filter_by_publisher()` - Filter by publisher names
- `st_filter_by_sdk()` - Filter by SDK usage
- `st_filter_by_date()` - Filter by release date
- `st_get_filter_collection()` - Pre-built filter collections
- `st_combine_filters()` - Combine multiple filters

## Quick Examples

```r
# Find Word games
word_filter <- st_create_simple_filter("Game Sub-genre", "Word")

# Get top Word games by DAU
word_games <- st_get_filtered_apps(
  filter_id = word_filter,
  measure = "DAU",
  regions = "US"
)

# Filter F2P games with ads
f2p_filter <- st_filter_by_monetization(
  free_only = TRUE,
  has_ads = TRUE
)

# Get Unity-based games
unity_filter <- st_filter_by_sdk("Unity")
```

## Integration with Existing Functions

Custom filters work seamlessly with:
- `st_top_charts()` - Use `category = 0` with `custom_fields_filter_id`
- `st_metrics()` - Get metrics for filtered apps
- `st_sales_report()` - Sales data for filtered apps

## Key Features

### Robust Error Handling
- Input validation for all filter criteria
- Automatic defaults for optional parameters
- Clear error messages for invalid fields
- Graceful handling of API failures

### Performance Optimizations
- Filter IDs are cached and reusable
- Identical filters return same ID (deduplication)
- Batch operations for multiple filters
- Pagination support for large result sets

### Comprehensive Testing
- Unit tests for all functions
- Integration tests with live API
- Edge case handling (boolean fields, empty values)
- Validation of filter combinations

## Common Use Cases

1. **Market Segmentation**
   - Analyze specific game genres
   - Compare monetization models
   - Track new vs established apps

2. **Competitive Intelligence**
   - Monitor apps from specific publishers
   - Track apps using certain SDKs
   - Analyze high-retention apps

3. **Trend Analysis**
   - New releases by date range
   - Platform-exclusive apps
   - Editor's Choice apps

## Best Practices

1. **Cache Filter IDs**: Save commonly used filter IDs for reuse
2. **Validate Filters**: Always verify filter contents with `st_custom_fields_filter_by_id()`
3. **Use Collections**: Leverage pre-built filter collections for common analyses
4. **Combine Strategically**: Create complex filters by combining simple ones

## Documentation

- Comprehensive vignette: `vignette("custom-fields", package = "sensortowerR")`
- Function documentation: `?st_custom_fields_filter`
- Examples in each function's help page

## Testing

Run tests with:
```r
devtools::test(filter = "custom-fields")
```

## Known Filter IDs

- Word games: `603697f4241bc16eb8570d37`
- Puzzle games: `601316bb241bc16eb8641a24`
- (IDs are permanent and can be hardcoded)

## Technical Notes

- Filters use MongoDB ObjectId format (24-character hex)
- Boolean fields require `true` parameter instead of values
- Category must be 0 when using custom filters
- Use `custom_tags_mode = "include_unified_apps"` for unified OS