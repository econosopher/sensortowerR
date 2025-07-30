# Custom Fields Filter Guide for sensortowerR

## Overview

Custom field filters are a powerful feature in Sensor Tower that allows users to create complex, reusable filters in the web interface and then apply them via the API using a unique filter ID.

## How Custom Field Filters Work

### 1. Filter Creation Process
1. User logs into Sensor Tower web interface (app.sensortower.com)
2. Navigates to the data section (e.g., Top Apps, Market Analysis)
3. Configures filters using the UI:
   - Categories (e.g., specific game genres)
   - Publishers (include/exclude specific publishers)
   - Custom tags/fields
   - Geographic regions
   - Platform specifications
   - Time ranges
   - Revenue/download thresholds
4. The web interface generates a unique filter ID
5. This ID appears in the URL as `custom_fields_filter_id=XXXXXXXXXX`

### 2. Filter ID Structure
- Format: 24-character hexadecimal string (e.g., "687df26ac5a19ebcfe817d7f")
- Persistent: Filter IDs remain valid as long as the filter exists in your account
- Account-specific: Filter IDs are tied to your Sensor Tower account
- Shareable within organization: Team members with the same account access can use the same filter IDs

### 3. API Implementation

The filter ID replaces the need to specify individual filter parameters in API calls:

```r
# Instead of this complex filter specification:
st_top_charts(
  category = 6014,
  regions = c("US", "GB", "JP"),
  # ... many other filter parameters
)

# You can use:
st_top_charts(
  custom_fields_filter_id = "687df26ac5a19ebcfe817d7f",
  regions = "US"  # Still need to specify regions
)
```

## Supported Endpoints

Based on API documentation analysis, `custom_fields_filter_id` is supported in:

### 1. Market Analysis Endpoints
- `/v1/{os}/sales_report_estimates_comparison_attributes` (used by `st_top_charts()`)
- Supports revenue and download metrics
- Can be combined with time range specifications

### 2. Top and Trending Endpoints
- `/v1/{os}/top_and_trending/active_users` (DAU/WAU/MAU metrics)
- Potentially other trending endpoints

## Important Requirements

### 1. Platform-Specific Behavior
- **iOS/Android**: Use `custom_fields_filter_id` directly
- **Unified OS**: Must also provide `custom_tags_mode` parameter
  - Options: "include", "exclude", "include_unified_apps"
  - This determines how the filter applies to unified apps

### 2. Parameter Interactions
- When `custom_fields_filter_id` is provided:
  - `category` becomes optional (filter already contains category info)
  - `regions` is still required (can override filter's region)
  - Time parameters (`date`, `time_range`) still apply
  - `limit` and `offset` work normally

### 3. Error Handling
Common errors and their causes:
- **422 Error**: Invalid filter ID or filter doesn't exist
- **403 Error**: Filter belongs to different account
- **400 Error**: Missing required parameters (e.g., `custom_tags_mode` for unified)

## Testing Strategy

### 1. Validation Tests
```r
# Test 1: Verify filter ID format
is_valid_filter_id <- function(id) {
  grepl("^[a-f0-9]{24}$", id)
}

# Test 2: Compare filtered vs unfiltered results
test_filter_effectiveness <- function(filter_id) {
  # Get data with filter
  filtered <- st_top_charts(
    custom_fields_filter_id = filter_id,
    regions = "US"
  )
  
  # Get unfiltered data
  unfiltered <- st_top_charts(
    category = 6014,  # Games
    regions = "US"
  )
  
  # Filter should return subset
  expect_true(nrow(filtered) <= nrow(unfiltered))
}
```

### 2. Integration Tests
```r
# Test different OS types
test_os_compatibility <- function(filter_id) {
  # iOS test
  ios_result <- tryCatch(
    st_top_charts(
      os = "ios",
      custom_fields_filter_id = filter_id,
      regions = "US"
    ),
    error = function(e) NULL
  )
  
  # Android test
  android_result <- tryCatch(
    st_top_charts(
      os = "android", 
      custom_fields_filter_id = filter_id,
      regions = "US"
    ),
    error = function(e) NULL
  )
  
  # Unified test (requires custom_tags_mode)
  unified_result <- tryCatch(
    st_top_charts(
      os = "unified",
      custom_fields_filter_id = filter_id,
      custom_tags_mode = "include",
      regions = "US"
    ),
    error = function(e) NULL
  )
  
  list(
    ios = !is.null(ios_result),
    android = !is.null(android_result),
    unified = !is.null(unified_result)
  )
}
```

## Best Practices

### 1. Filter Management
- Document filter IDs with their purpose
- Store filter IDs in configuration files
- Create a mapping of filter names to IDs:

```r
# config/filters.R
SENSOR_TOWER_FILTERS <- list(
  hypercasual_games = "687df26ac5a19ebcfe817d7f",
  top_publishers = "789ef34bc6b20fcade928e8a",
  strategy_games_asia = "456ab78cd9e31gbcef103f2b"
)
```

### 2. Error Recovery
```r
safe_filter_query <- function(filter_id, ...) {
  result <- tryCatch(
    st_top_charts(
      custom_fields_filter_id = filter_id,
      ...
    ),
    error = function(e) {
      if (grepl("422", e$message)) {
        warning("Invalid filter ID, falling back to category filter")
        st_top_charts(category = 6014, ...)
      } else {
        stop(e)
      }
    }
  )
  result
}
```

### 3. Performance Optimization
- Cache results when using the same filter repeatedly
- Filters are processed server-side, so complex filters don't impact API performance
- Use filters for commonly repeated queries to reduce parameter complexity

## Future Integration Ideas

### 1. Filter Discovery Function
```r
# Proposed function to validate and describe filters
st_describe_filter <- function(filter_id) {
  # Make a minimal API call to test the filter
  # Return information about what the filter does
}
```

### 2. Filter Builder Helper
```r
# Help users construct filter URLs
st_filter_url <- function(categories = NULL, 
                         publishers = NULL,
                         regions = NULL,
                         ...) {
  base_url <- "https://app.sensortower.com/top-charts"
  # Build URL with parameters
  # User can then create filter in web UI
}
```

### 3. Multi-Filter Support
```r
# Allow combining multiple filters
st_top_charts(
  custom_fields_filter_ids = c("filter1", "filter2"),
  filter_mode = "intersection"  # or "union"
)
```

## Known Limitations

1. **Filter Opacity**: API doesn't return filter details, only applies them
2. **No CRUD Operations**: Can't create/update/delete filters via API
3. **Account Dependency**: Filters are account-specific, not portable
4. **Limited Documentation**: Sensor Tower doesn't provide detailed filter specs

## Recommendations for Package Enhancement

1. **Add filter validation**: Check format before API calls
2. **Improve error messages**: Provide guidance when filter fails
3. **Add examples**: Show common filter use cases
4. **Cache filter results**: Store successful filter IDs
5. **Documentation**: Add filter guide to package vignettes