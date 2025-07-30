# Sensor Tower Web Interface to API Parameter Mapping

## URL Analysis

From the provided URL:
```
https://app.sensortower.com/market-analysis/top-apps?os=unified&edit=1&granularity=weekly&start_date=2025-06-29&end_date=2025-07-28&duration=P30D&measure=DAU&comparison_attribute=absolute&country=US&country=AU...&category=0&device=iphone&device=ipad&device=android&metric=downloads&page=1&page_size=25&custom_fields_filter_mode=include_unified_apps&period=day
```

## Parameter Breakdown

### 1. Core Parameters (Direct Mapping)
| Web Parameter | API Parameter | Notes |
|--------------|---------------|-------|
| `os=unified` | `os` | Direct mapping |
| `measure=DAU` | `measure` | Direct mapping (DAU, WAU, MAU, revenue, units) |
| `comparison_attribute=absolute` | `comparison_attribute` | Direct mapping |
| `start_date=2025-06-29` | `date` or `start_date` | Depends on endpoint |
| `end_date=2025-07-28` | `end_date` | Optional in API |
| `category=0` | `category` | 0 means "All Categories" |
| `page_size=25` | `limit` | API uses `limit` instead |
| `page=1` | `offset` | API uses offset (calculate as (page-1)*limit) |

### 2. Country Parameter (Special Handling)
- **Web**: Multiple `country=XX` parameters (appears to be ~100 countries)
- **API**: `regions` parameter accepts comma-separated string or array
- **Note**: The URL shows virtually ALL countries, which might be equivalent to "WW" (worldwide)

### 3. Granularity/Time Parameters (Complex Mapping)
| Web Parameter | Purpose | API Equivalent |
|--------------|---------|----------------|
| `granularity=weekly` | Data aggregation level | `time_range` in API |
| `duration=P30D` | ISO 8601 duration (30 days) | Calculated from start/end dates |
| `period=day` | Unknown - conflicts with granularity? | Possibly ignored |

### 4. Device Parameters (Web-Specific)
- **Web**: `device=iphone&device=ipad&device=android`
- **API**: Uses `device_type` with values: "iphone", "ipad", "total", or none for Android
- **Note**: Having all devices selected in web likely maps to `os=unified` without device restrictions

### 5. Custom Fields Filter Mode
- **Web**: `custom_fields_filter_mode=include_unified_apps`
- **API**: `custom_tags_mode` with same values
- **Purpose**: Controls how unified apps are filtered when using custom filters

### 6. Parameters Not in API
- `edit=1` - UI state parameter (editing mode)
- `metric=downloads` - Seems redundant with `measure=DAU`
- `duration=P30D` - API calculates from date range

## Implementation Recommendations

### 1. Add Country Helper Function
```r
# Convert extensive country list to regions parameter
st_parse_countries <- function(countries) {
  # If more than 50 countries, suggest using "WW"
  if (length(countries) > 50) {
    message("Large country list detected. Consider using 'WW' for worldwide.")
    return("WW")
  }
  # Otherwise return comma-separated
  paste(countries, collapse = ",")
}
```

### 2. Handle "All Categories"
```r
# In st_top_charts and other functions
if (!is.null(category) && category == "0") {
  category <- NULL  # Let API return all categories
  message("Category '0' (All Categories) converted to NULL")
}
```

### 3. Support custom_fields_filter_mode Parameter
The `custom_fields_filter_mode` is already supported as `custom_tags_mode` in the API, but we should document the mapping:

```r
# Web parameter name -> API parameter name
web_to_api_params <- list(
  custom_fields_filter_mode = "custom_tags_mode",
  page_size = "limit",
  country = "regions"
)
```

### 4. URL Parser Function
```r
#' Parse Sensor Tower Web URL into API Parameters
#' 
#' @param url Character string. Sensor Tower web interface URL
#' @return List of API-compatible parameters
#' @export
st_parse_web_url <- function(url) {
  parsed <- httr::parse_url(url)
  query <- parsed$query
  
  # Initialize API parameters
  api_params <- list()
  
  # Direct mappings
  if (!is.null(query$os)) api_params$os <- query$os
  if (!is.null(query$measure)) api_params$measure <- query$measure
  if (!is.null(query$comparison_attribute)) {
    api_params$comparison_attribute <- query$comparison_attribute
  }
  if (!is.null(query$start_date)) api_params$date <- query$start_date
  if (!is.null(query$end_date)) api_params$end_date <- query$end_date
  if (!is.null(query$granularity)) {
    api_params$time_range <- switch(
      query$granularity,
      "daily" = "day",
      "weekly" = "week",
      "monthly" = "month",
      "quarterly" = "quarter",
      query$granularity
    )
  }
  
  # Category (handle "0" as all categories)
  if (!is.null(query$category)) {
    if (query$category != "0") {
      api_params$category <- query$category
    }
  }
  
  # Countries (handle multiple values)
  countries <- query[names(query) == "country"]
  if (length(countries) > 0) {
    api_params$regions <- if (length(countries) > 50) "WW" else paste(countries, collapse = ",")
  }
  
  # Page/limit
  if (!is.null(query$page_size)) api_params$limit <- as.integer(query$page_size)
  if (!is.null(query$page)) {
    page <- as.integer(query$page)
    limit <- as.integer(query$page_size %||% 25)
    api_params$offset <- (page - 1) * limit
  }
  
  # Custom fields filter mode -> custom tags mode
  if (!is.null(query$custom_fields_filter_mode)) {
    api_params$custom_tags_mode <- query$custom_fields_filter_mode
  }
  
  # Extract custom filter ID if present
  if (!is.null(query$custom_fields_filter_id)) {
    api_params$custom_fields_filter_id <- query$custom_fields_filter_id
  }
  
  api_params
}
```

## Example Usage

```r
# Parse the provided URL
url <- "https://app.sensortower.com/market-analysis/top-apps?os=unified&..."
params <- st_parse_web_url(url)

# Use in API call
do.call(st_top_charts, params)
```

## Key Differences Between Web and API

1. **Country Selection**: Web allows individual country selection, API prefers regions or comma-separated list
2. **Pagination**: Web uses page numbers, API uses offset
3. **Time Specification**: Web has redundant time parameters, API is more streamlined
4. **Device Filtering**: Web has explicit device checkboxes, API handles via os/device_type
5. **UI State**: Web URLs contain UI-specific parameters not needed for API

## Recommendations

1. **Implement URL parser**: Add `st_parse_web_url()` to help users transition from web to API
2. **Document parameter mappings**: Add this mapping to package documentation
3. **Handle edge cases**: All categories (0), worldwide (many countries), etc.
4. **Validate combinations**: Some parameter combinations may not be valid in the API