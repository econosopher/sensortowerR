# Custom Field Filter Investigation Summary

## Overview
This document summarizes the comprehensive investigation into Sensor Tower's custom field filter functionality and its integration into the sensortowerR package.

## Key Findings

### 1. How Custom Filters Work
- **Creation**: Users create filters in the Sensor Tower web interface (app.sensortower.com)
- **Format**: 24-character hexadecimal string (e.g., "687df26ac5a19ebcfe817d7f")
- **Persistence**: Filter IDs remain valid as long as they exist in the account
- **Account-specific**: Filters are tied to specific Sensor Tower accounts

### 2. API Support
Custom field filters are supported in:
- **Market Analysis endpoints**: `/v1/{os}/sales_report_estimates_comparison_attributes`
- **Top and Trending endpoints**: Including active user metrics
- Currently implemented in `st_top_charts()` function

### 3. Implementation Requirements
- **iOS/Android**: Use `custom_fields_filter_id` parameter directly
- **Unified OS**: Must also provide `custom_tags_mode` parameter
  - Options: "include", "exclude", "include_unified_apps"
- **Category**: Becomes optional when custom filter is provided
- **Other parameters**: Still apply (regions, time_range, etc.)

## What Was Created

### 1. Documentation
- **Custom Fields Filter Guide** (`inst/docs/custom_fields_filter_guide.md`)
  - Comprehensive explanation of how filters work
  - API implementation details
  - Testing strategies
  - Best practices

- **Integration Strategy** (`inst/docs/custom_filter_integration_strategy.md`)
  - Roadmap for enhanced filter support
  - Proposed helper functions
  - Implementation phases
  - Success metrics

### 2. Code Enhancements

#### A. Updated `st_top_charts()` Function
- Made `category` parameter optional when `custom_fields_filter_id` is provided
- Improved error messages for better user guidance

#### B. New Helper Functions (`R/st_filter_helpers.R`)
```r
# Validate filter ID format
st_is_valid_filter_id("687df26ac5a19ebcfe817d7f")

# Test if filter works with API
st_test_filter("687df26ac5a19ebcfe817d7f")

# Generate example IDs for testing
st_generate_example_filter_ids(5)

# Build URL for creating filters
st_build_filter_url(os = "ios", category = 6014)

# Extract filter ID from URL
st_extract_filter_id("https://app.sensortower.com/...?custom_fields_filter_id=...")

# Compare filtered vs unfiltered results
st_compare_filter_results(filter_id, category = 6014)
```

### 3. Examples and Tests

#### A. Basic Usage Example (`examples/custom_filter_example.R`)
- Demonstrates using custom filters with different OS types
- Shows parameter requirements
- Includes visualization example
- Provides troubleshooting guidance

#### B. Comprehensive Test Suite (`inst/test_scripts/test_custom_field_filters.R`)
- Format validation tests
- API functionality tests
- Parameter interaction tests
- Different measure types (revenue, downloads, DAU/WAU/MAU)
- Time range variations
- Error handling tests

#### C. Filter Discovery Helper (`examples/find_test_filter_ids.R`)
- Methods to obtain valid filter IDs
- URL generation for filter creation
- Configuration file templates
- Testing utilities

### 4. Updated Package Documentation
- Enhanced README with custom filter section
- Updated function documentation
- Added to NEWS.md for version 0.7.3

## Testing Results

### What Works
✅ Filter ID format validation  
✅ iOS/Android filter support  
✅ Unified OS with custom_tags_mode  
✅ Different measures (revenue, units, active users)  
✅ Various time ranges  
✅ Parameter validation  

### Known Issues
❌ Example filter ID (687df26ac5a19ebcfe817d7f) may not work with all accounts  
❌ No way to retrieve filter details via API  
❌ Filters are opaque - can't see what they're filtering  

## Best Practices

### 1. Filter Management
```r
# Store filter IDs in configuration
FILTERS <- list(
  hypercasual = "687df26ac5a19ebcfe817d7f",
  strategy_asia = "789ef34bc6b20fcade928e8a"
)

# Use descriptive names
top_games <- st_top_charts(
  custom_fields_filter_id = FILTERS$hypercasual,
  regions = "US"
)
```

### 2. Error Handling
```r
# Always validate before use
if (st_is_valid_filter_id(filter_id)) {
  result <- st_test_filter(filter_id)
  if (result$success) {
    # Use the filter
  }
}
```

### 3. Documentation
- Document what each filter does
- Include the web UI configuration
- Note the creation date
- Track which analyses use which filters

## Future Enhancements

### Phase 1: Immediate (Completed)
- ✅ Basic filter support in st_top_charts()
- ✅ Helper functions for validation
- ✅ Documentation and examples

### Phase 2: Short-term
- [ ] Extend filter support to other compatible functions
- [ ] Add filter caching mechanism
- [ ] Create vignette on custom filters

### Phase 3: Long-term
- [ ] Filter discovery tools
- [ ] Multi-filter support
- [ ] Filter sharing utilities
- [ ] Integration with other Sensor Tower features

## How to Get Started

### For Users
1. Log into app.sensortower.com
2. Create filters in the web interface
3. Copy the filter ID from the URL
4. Use in R:
```r
library(sensortowerR)

# Test your filter
st_test_filter("your_filter_id_here")

# Use in analysis
data <- st_top_charts(
  custom_fields_filter_id = "your_filter_id_here",
  regions = "US"
)
```

### For Developers
1. Review the helper functions in `R/st_filter_helpers.R`
2. See integration strategy document for enhancement ideas
3. Run test suite to understand functionality
4. Contribute improvements via pull requests

## Conclusion

Custom field filters provide a powerful way to leverage Sensor Tower's web interface filtering capabilities in programmatic workflows. The implementation in sensortowerR now provides basic support with room for significant enhancements based on user needs and API capabilities.