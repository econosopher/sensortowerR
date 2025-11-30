# sensortowerR

<p align="center"><img src="inst/images/sensortowerR_sticker.png" width="200"></p>

An R package for interfacing with the Sensor Tower API to fetch mobile app analytics data, including app info, publisher details, revenue/download estimates, active user metrics, and professional dashboard generation.

## What's New

### v0.8.4 - ID Resolution Bug Fix & Documentation Update
- **Critical Fix**: Fixed `unified_app_id` resolution in `st_sales_report()` and `st_batch_metrics()`
  - Previously, passing `unified_app_id` would fail to resolve platform-specific IDs
  - Now correctly looks up and uses `ios_app_id` or `android_app_id` as needed
- **Cache Consistency**: Fixed ID cache to use consistent field naming (`ios_app_id` not `ios_id`)
- **Documentation Update**: Updated README with validated workflow for finding and fetching app data
  - Clear step-by-step guide: search by name → get IDs → fetch data
  - Data availability matrix showing which functions provide what data
  - Explicit notes on retention/demographics limitations

### v0.8.3 - Robustness & Refactoring
- **Core Refactoring**: Streamlined internal logic for `st_batch_metrics` and `st_metrics` for better maintainability and performance.
- **Enhanced Stability**: Improved ID resolution consistency across all functions.
- **Expanded Testing**: Comprehensive test coverage for batch processing and edge cases.

### v0.7.6 - Critical Fix for DAU/WAU/MAU Sorting with Custom Filters
- **BREAKING FIX**: DAU/WAU/MAU measures now correctly use the sales endpoint
  - Previously, these measures incorrectly routed to `active_users` endpoint
  - This caused incorrect sorting when using custom filters from web interface
  - Now all measures use `sales_report_estimates_comparison_attributes` endpoint
- **Custom Filter URL Support**: 
  - Copy URLs directly from Sensor Tower web interface
  - Extract `custom_fields_filter_id` from the 'uai' parameter
  - Works correctly with DAU sorting when using `measure = "revenue"`
- **Example - Using Custom Filter URLs**:
  ```r
  # URL from Sensor Tower: ...&uai=5a39e9681454d22f5a5e75ca&...
  # Extract the custom_fields_filter_id and use:
  top_word_puzzles <- st_top_charts(
    os = "unified",
    measure = "revenue",  # Use revenue endpoint, custom filter handles DAU
    custom_fields_filter_id = "5a39e9681454d22f5a5e75ca",
    custom_tags_mode = "include_unified_apps",
    category = 7019,  # Still required with custom filter
    regions = "US",
    date = "2025-07-20",
    end_date = "2025-08-18"
  )
  # Results will be sorted by DAU as shown in web interface
  ```

### v0.7.5 - Custom Filter Support Across Functions
- **Enhanced Custom Filter Integration**: 
  - `st_category_rankings()` now supports custom field filters from Sensor Tower web interface
  - Use `custom_fields_filter_id` parameter to apply your saved filters
  - Category parameter is now optional when using custom filters
  - Full support for unified OS with `custom_tags_mode` parameter
- **Reusable Filter Workflow**:
  - Create filters in the Sensor Tower web interface
  - Use the same filter ID across multiple R functions
  - Seamlessly integrate web-based analysis with R workflows
- **Example**:
  ```r
  # Use custom filter instead of category
  rankings <- st_category_rankings(
    os = "ios",
    custom_fields_filter_id = "60746340241bc16eb8a65d76",
    chart_type = "topgrossingapplications"
  )
  ```

### v0.6.0 - Year-over-Year Metrics
- **New Function**: `st_yoy_metrics()` - Flexible year-over-year comparisons
  - Compare any date range across multiple years (e.g., Q1, holiday season, custom periods)
  - Automatic YoY calculations with both percentage and absolute changes
  - Works with Date objects or "MM-DD" format for easy period specification
  - Smart leap year handling (Feb 29 automatically adjusted)
  - Defaults to current and previous year when years not specified
- **New Helper**: `calculate_yoy_growth()` - Calculate growth rates from baseline year
  - Compute growth relative to any baseline year
  - Returns both percentage growth and index values (base year = 100)
  - Perfect for multi-year trend analysis
- **Example Use Cases**:
  ```r
  # Compare Q1 performance across 3 years
  q1_metrics <- st_yoy_metrics(
    os = "ios",
    ios_app_id = "553834731",
    years = c(2022, 2023, 2024),
    period_start = "01-01",
    period_end = "03-31",
    countries = "US",
    granularity = "monthly"
  )
  
  # Compare holiday season with automatic YoY calculations
  holiday_metrics <- st_yoy_metrics(
    os = "unified",
    unified_app_id = "5ba4585f539ce75b97db6bcb",
    period_start = "11-01",
    period_end = "12-31",
    countries = c("US", "GB", "JP"),
    granularity = "daily"
  )
  ```

### v0.5.1 - Strict ID Validation
- **Unified ID Validation**: `unified_app_id` now strictly validates input format
  - Only accepts 24-character hexadecimal IDs (e.g., "5ba4585f539ce75b97db6bcb")
  - Rejects iOS numeric IDs and Android package names with clear error messages
  - Guides users to use correct parameters (`ios_app_id` or `android_app_id`)
- **Platform-Specific Parameters**: Enhanced support for single-platform requests
  - `ios_app_id` and `android_app_id` work correctly with all metrics including MAU
  - Properly handles NA values when only one platform is specified
  - Fixed entity creation bugs for platform-specific requests
- **Improved Error Messages**: Clear guidance when wrong ID types are used
  - Tells users exactly what format is expected
  - Suggests the correct parameter for their ID type
  - No more confusion about which parameter to use

### v0.5.0 - ID Optimization & Caching
- **Smart ID Caching**: Dramatically reduce API calls with intelligent ID caching
  - Persistent cache stores app ID mappings for 30 days
  - Batch API calls minimize redundant lookups
  - Cache persists between R sessions
- **New Function**: `st_smart_metrics()` - Fetch metrics with ID type detection
  - Accepts mixed ID types (iOS, Android, unified hex)
  - Uses cache to avoid redundant lookups
  - Groups compatible requests for efficiency
- **ID Type Detection**: Automatically identifies ID formats
  - iOS numeric (e.g., "553834731")
  - Android package (e.g., "com.king.candycrushsaga")
  - Unified hex (e.g., "5ba4585f539ce75b97db6bcb")
- **Cache Management Functions**:
  - `st_cache_info()` - View cache statistics and contents
  - `st_clear_id_cache()` - Clear the ID cache
- **Configuration Options**:
  ```r
  # Enable/disable caching (default: TRUE)
  options(sensortowerR.use_cache = TRUE)
  
  # Set cache expiry (default: 30 days)
  options(sensortowerR.cache_max_age_days = 30)
  
  # Enable verbose mode to see optimization in action
  options(sensortowerR.verbose = TRUE)
  ```

### v0.4.2
- **New Function**: `st_get_unified_mapping()` - Get complete ID mapping between platforms
  - Retrieves the mapping between platform-specific and unified app IDs
  - Uses the comparison attributes endpoint for rich entity data
  - Falls back to `st_app_lookup()` when needed
  - Returns unified_app_id, ios_app_id, android_app_id, and publisher info
- **Enhanced ID Tracking**: Functions now preserve original input IDs
  - `st_batch_metrics()` maintains `original_id` throughout processing
  - Helps with downstream analysis when using specific ID types

### v0.4.1
- **New Function**: `st_app_lookup()` - Resolve platform IDs from unified IDs
  - Accepts Sensor Tower hex IDs, iOS numeric IDs, or Android package names
  - Returns both iOS and Android IDs for use with other API functions
  - Helpful when unified endpoints don't work with certain ID types
- **New Function**: `st_api_diagnostics()` - Diagnose API issues systematically
  - Detects ID type and tests various endpoints
  - Provides specific recommendations for each app
  - Helps debug why certain IDs aren't working
- **New Function**: `st_batch_metrics()` - Efficiently fetch metrics for multiple apps
  - Groups compatible requests to minimize API calls
  - Supports parallel processing for large batches
  - **Full active user support**: Works with DAU, WAU, and MAU metrics
  - Perfect for dashboards and bulk analysis
- **Important Note**: For best results, always use platform-specific IDs when available
  - Use `ios_app_id` and `android_app_id` parameters for most accurate results
  - Platform-specific fetching correctly combines iOS and Android data

### v0.4.0
- **BREAKING CHANGE**: Key parameters now required instead of defaulting
  - `countries` must be explicitly specified (no default to "US")
  - `date_granularity` must be explicitly specified (no default to "daily")
  - `os` must be explicitly specified (no default to "unified")
  - `start_date` and `end_date` must be explicitly specified
- **Test Improvements**: Consolidated test scripts to speed up CRAN checks
- **Package Cleanup**: Fixed R CMD check warnings and added missing imports

### v0.2.2
- **Country parameter update**: Made country a required parameter across all data-fetching functions

### v0.2.1
- **Console feedback**: All API functions now display the parameters being used, making it clear what data you're requesting
- **Transparency**: Shows default values like `country = "WW"` to avoid confusion about data scope

Example output:
```
=== Sensor Tower API Request ===
  Endpoint: Top Publishers
  Measure: revenue
  OS: unified
  Country: WW
  Time Range: month
  Date: 2025-06-01
  Category: 6014
  Limit: 10
  Include Apps: TRUE
================================
```

## IMPORTANT: Best Practices for Finding App Data

### DO: Search for apps by name, then fetch their data

The **validated workflow** for getting data about specific apps (v0.8.4+):

```r
# Step 1: Search for apps by name to get their unified IDs
apps <- st_app_info("Royal Match")
unified_id <- apps$unified_app_id[1]  # 24-char hex: "5f16a8019f7b275235017614"

# Step 2: (Optional) Get platform-specific IDs
platform_ids <- st_app_lookup(unified_id)
# Returns: ios_app_id = 1482155847, android_app_id = "com.dreamgames.royalmatch"

# Step 3: Fetch time-series revenue/downloads
# Option A: Use unified_app_id directly (auto-resolves to platform IDs)
revenue <- st_sales_report(
  os = "ios",  # or "android"
  unified_app_id = unified_id,
  countries = "US",
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  date_granularity = "monthly"
)

# Option B: Use platform-specific IDs directly
revenue <- st_sales_report(
  os = "ios",
  ios_app_id = platform_ids$ios_app_id,
  countries = "US",
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  date_granularity = "monthly"
)

# Step 4: Fetch time-series MAU/DAU/WAU
active_users <- st_batch_metrics(
  os = "ios",
  app_list = unified_id,
  metrics = c("mau", "dau"),
  date_range = list(start_date = "2024-01-01", end_date = "2024-12-31"),
  countries = "US",
  granularity = "monthly"
)
```

### DON'T: Use st_top_charts() to find specific apps

**Never use this pattern:**
```r
# BAD: Hoping your apps are in the top 1000
top_apps <- st_top_charts(category = "6014", limit = 1000)
my_apps <- filter(top_apps, name %in% c("Royal Match", "Candy Crush"))
```

This approach will fail because:
1. Your target apps may not be in the top N results
2. It wastes API calls fetching irrelevant data
3. The ranking changes daily - your app may drop out
4. Smaller/newer apps will never appear

### Correct Workflow for Multiple Apps

```r
library(dplyr)
library(purrr)

# Step 1: Search for each app and get IDs
game_names <- c("Royal Match", "Candy Crush", "Homescapes")

app_lookup <- map_dfr(game_names, function(name) {
  result <- st_app_info(name, limit = 5)
  if (nrow(result) > 0) {
    tibble(
      search_term = name,
      app_name = result$unified_app_name[1],
      unified_app_id = result$unified_app_id[1]
    )
  }
})

# Step 2: Resolve platform IDs for each app
app_lookup <- app_lookup %>%
  mutate(
    platform_ids = map(unified_app_id, st_app_lookup),
    ios_app_id = map_chr(platform_ids, ~ .x$ios_app_id %||% NA_character_),
    android_app_id = map_chr(platform_ids, ~ .x$android_app_id %||% NA_character_)
  )

# Step 3: Fetch revenue/downloads for each app
all_data <- map2_dfr(app_lookup$unified_app_id, app_lookup$app_name, function(id, name) {
  result <- st_sales_report(
    os = "ios",
    unified_app_id = id,
    countries = c("US", "GB"),
    start_date = "2024-01-01",
    end_date = "2024-12-31",
    date_granularity = "monthly"
  )
  result$app_name <- name
  result
})

# Step 4: Fetch MAU/DAU time series
user_metrics <- st_batch_metrics(
  os = "unified",
  app_list = app_lookup$unified_app_id,
  metrics = c("mau", "dau"),
  date_range = list(start_date = "2024-01-01", end_date = "2024-12-31"),
  countries = "US",
  granularity = "monthly"
)
```

### Data Types and Their Appropriate Functions

| Data Type | Function | Works With | Notes |
|-----------|----------|------------|-------|
| **App Search** | `st_app_info()` | app name | Returns unified_app_id |
| **ID Resolution** | `st_app_lookup()` | any ID type | Returns all ID types |
| **Revenue/Downloads** | `st_sales_report()` | unified_app_id, ios_app_id, android_app_id | Time-series by country/platform |
| **Time-series DAU/WAU/MAU** | `st_batch_metrics()` | unified_app_id, platform IDs | Active users over time |
| **Snapshot Retention/Demographics** | `st_app_enriched()` | unified_app_id only | NOT time-series, snapshot only |
| **Top Charts** | `st_top_charts()` | category | Market analysis, NOT finding specific apps |

### Data Availability Summary

| Data Type | Endpoint | Time-Series? | Region Coverage |
|-----------|----------|--------------|-----------------|
| Revenue/Downloads | `st_sales_report()` | ✅ Yes | All countries |
| MAU/DAU/WAU | `st_batch_metrics()` | ✅ Yes | All countries |
| Retention (D1-D60) | `st_app_enriched()` | ❌ Snapshot | US, WW |
| Demographics | `st_app_enriched()` | ❌ Snapshot | US only |

### Retention & Demographics Limitations

- **Retention cohorts available**: D1, D7, D14, D30, D60 only (**D90 is NOT available**)
- **Demographics (age/gender)**: Primarily available for **US market only**
- **Enriched metrics**: These are **snapshot aggregates**, not time-series data
- **Smaller apps**: May not have retention/demographic data available
- **No direct API endpoint**: Retention/demographics come from `aggregate_tags` in comparison endpoint

## Installation

```r
# Install from GitHub
devtools::install_github("econosopher/sensortowerR")
```

## Authentication

Store your Sensor Tower API token as an environment variable:

```r
# Edit your R environment file
usethis::edit_r_environ()

# Add this line (replace with your actual token):
# SENSORTOWER_AUTH_TOKEN="YOUR_SECRET_TOKEN_HERE"

# Restart R session for changes to take effect
```

The package automatically uses the `SENSORTOWER_AUTH_TOKEN` environment variable.

## Important API Limitations

### Batch API Capabilities

Many Sensor Tower endpoints support batch requests, allowing you to fetch data for multiple apps in a single API call:

1. **Endpoints Supporting Batch Requests**:
   - ✅ **iOS Sales Report** (`/v1/ios/sales_report_estimates`) - Pass multiple app IDs
   - ✅ **Android Sales Report** (`/v1/android/sales_report_estimates`) - Pass multiple app IDs
   - ❌ **App Details** - Only returns first app
   - ❌ **Active Users** - Does not support multiple IDs
   - ❌ **Category Rankings** - Single app only

2. **Using Batch Requests**:
   ```r
   # NEW: Consistent parameter style (like st_metrics)
   revenue_data <- st_sales_report(
     os = "ios",
     ios_app_id = "1195621598",  # Single app with specific parameter
     countries = "US",
     start_date = Sys.Date() - 30,
     end_date = Sys.Date() - 1,
     date_granularity = "daily"
   )
   
   # For batch requests (multiple apps), use multiple calls or purrr::map
   # Each parameter only accepts a single ID
   app_ids <- c("1195621598", "553834731", "1053012308")
   batch_revenue <- purrr::map_dfr(app_ids, ~ {
     st_sales_report(
       os = "ios",
       ios_app_id = .x,
       countries = "US",
       start_date = Sys.Date() - 30,
       end_date = Sys.Date() - 1,
       date_granularity = "daily"
     )
   })
   ```

3. **Performance Benefits**:
   - Reduces API calls by up to 90% for multi-app analyses
   - Faster execution time
   - Lower risk of hitting rate limits
   - More efficient data processing

### Custom Filter Support

sensortowerR now supports using custom filters created in the Sensor Tower web interface:

1. **Supported Functions**:
   - ✅ `st_top_charts()` - Get detailed metrics for filtered apps
   - ✅ `st_category_rankings()` - Get store rankings for filtered apps
   - ✅ `st_sales_report()` - Get revenue/downloads for filtered apps

2. **How to Use**:
   ```r
   # Get your filter ID from the web interface URL
   filter_id <- "60746340241bc16eb8a65d76"
   
   # Use with any supported function
   rankings <- st_category_rankings(
     os = "ios",
     custom_fields_filter_id = filter_id,  # No category needed!
     chart_type = "topgrossingapplications"
   )
   
   # For unified OS, specify custom_tags_mode
   unified_data <- st_top_charts(
     os = "unified",
     custom_fields_filter_id = filter_id,
     custom_tags_mode = "include_unified_apps"
   )
   ```

3. **Benefits**:
   - Reuse complex filters from web interface
   - No need to manually list app IDs
   - Filters automatically update as you modify them on the web
   - Seamless integration between web and R workflows

See the [Custom Filters Guide](inst/docs/custom_filters_guide.md) for detailed documentation.

### Daily Data Limitations

When requesting daily granularity data from Sensor Tower's API:

1. **Unified endpoints are completely broken for sales_report_estimates**
   - The unified sales_report_estimates endpoint returns EMPTY for ALL granularities (daily, weekly, monthly, quarterly)
   - Platform-specific endpoints (iOS/Android) must ALWAYS be used instead
   - The `st_metrics()` function automatically handles this by using platform-specific endpoints

2. **Working Endpoint Matrix** (as of July 2025):
   ```
   ✅ iOS + daily/weekly/monthly/quarterly = SUCCESS
   ✅ Android + daily/weekly/monthly/quarterly = SUCCESS  
   ❌ Unified sales_report + ANY granularity = EMPTY RESPONSE
   ✅ Unified top_publishers = SUCCESS (but limited)
   ✅ iOS/Android active_users = SUCCESS (with correct params)
   ❌ Unified active_users = ERROR 422
   ```

3. **The `st_metrics()` function automatically handles this**:
   - Detects when daily data is requested
   - Switches to platform-specific endpoints transparently
   - Combines iOS and Android data for unified view
   - Shows progress messages to explain what's happening

### Monthly Data with Custom Date Ranges

When using `time_range = "month"` with the `st_top_publishers()` function, be aware that:

1. **Date Alignment Required**: Dates must align with period boundaries
   - For `month`: Must be first day of month (e.g., 2025-06-01)
   - For `quarter`: Must be quarter start (Jan 1, Apr 1, Jul 1, Oct 1)
   - For `year`: Must be January 1st
   - The function will error if dates don't align properly
   
2. **Automatic End Date**: When using monthly/quarterly/yearly time ranges:
   - If `end_date` is not provided, it's automatically set to the last day of the period
   - Example: `date = "2025-06-01", time_range = "month"` auto-sets `end_date` to "2025-06-30"
   - A message will inform you of the auto-calculated end_date
   
3. **Accurate Custom Date Ranges**: For precise date ranges that don't align with calendar months:
   - Use `time_range = "day"` and aggregate the results manually
   - This ensures you get exactly the date range you request

```r
# Example: Getting accurate 30-day revenue
daily_data <- map_df(seq(start_date, end_date, by = "day"), function(d) {
  st_top_publishers(
    time_range = "day",
    date = d,
    limit = 20
  )
})

# Aggregate by publisher
monthly_summary <- daily_data %>%
  group_by(publisher_id, publisher_name) %>%
  summarise(
    total_revenue = sum(revenue_usd),
    total_downloads = sum(units_absolute)
  )
```

## Category Codes

Common category codes for use with API functions:

### iOS Categories
- `6000` - Games (general)
- `6014` - Games
- `6016` - Social Networking
- `7001` - Action (games)
- `7002` - Adventure (games)
- `7012` - Casino (games)
- `7014` - Role Playing (games)

### Android Categories
- `GAME` - Games
- `SOCIAL` - Social
- `game` - Games (lowercase)
- `game_action` - Action Games
- `game_adventure` - Adventure Games
- `game_casino` - Casino Games
- `game_role_playing` - Role Playing Games

For a complete list, use `st_categories()` to see available categories.

## Using Custom Filters from Sensor Tower Web Interface

The package supports using custom filters created in the Sensor Tower web interface through the `custom_fields_filter_id` parameter:

```r
# Use a custom filter ID from the Sensor Tower web interface
top_apps <- st_top_charts(
  measure = "revenue",
  os = "unified",
  custom_fields_filter_id = "your_filter_id_here",
  custom_tags_mode = "include",  # Required when os = "unified"
  regions = "US",
  time_range = "month"
)
```

### How to Get a Custom Filter ID:
1. Log into the [Sensor Tower web interface](https://app.sensortower.com/)
2. Navigate to the Top Apps section
3. Configure your desired filters (categories, publishers, keywords, etc.)
4. The URL will contain `custom_fields_filter_id=YOUR_ID`
5. Copy that ID and use it in your API calls

**Note**: When using `custom_fields_filter_id`, the `category` parameter becomes optional since the filter already contains category information.

## Converting Web URLs to API Calls

The package can parse Sensor Tower web interface URLs and convert them to API parameters:

```r
# Parse a web URL
url <- "https://app.sensortower.com/market-analysis/top-apps?os=unified&measure=DAU&category=6014"
params <- st_parse_web_url(url)

# Use the parameters in an API call
data <- do.call(st_top_charts, params)

# Extract and analyze URL parameters
params_df <- st_extract_url_params(url)

# Build a web URL from API parameters
web_url <- st_build_web_url(
  os = "unified",
  measure = "revenue",
  category = 6014,
  regions = "US,GB"
)
```

### Parameter Mapping
- Web `page_size` → API `limit`
- Web `country` (multiple) → API `regions` (comma-separated)
- Web `granularity` → API `time_range`
- Web `custom_fields_filter_mode` → API `custom_tags_mode`
- Web category `0` (all) → API `category = NULL`

## Core Functions

- **`st_app_info()`**: Search for apps and get basic information
- **`st_app_lookup()`**: Resolve platform-specific IDs from unified IDs
- **`st_app_enriched()`**: **NEW!** Fetch retention, MAU, demographics for specific apps by ID
- **`st_api_diagnostics()`**: **NEW!** Diagnose why app IDs aren't working and get recommendations
- **`st_batch_metrics()`**: **NEW!** Efficiently fetch metrics for multiple apps with mixed ID types
- **`st_publisher_apps()`**: Get all apps from a specific publisher  
- **`st_metrics()`**: Detailed daily metrics for specific apps (see note below)
- **`st_top_charts()`**: Unified function for all top charts (revenue, downloads, DAU, WAU, MAU)
- **`st_game_summary()`**: Game market summary (aggregated downloads/revenue by categories and countries)
- **`st_category_rankings()`**: **NEW!** Get official app store rankings by category
- **`st_app_details()`**: **NEW!** Fetch comprehensive app metadata and store listings
- **`st_top_publishers()`**: **NEW!** Get top publishers by revenue or downloads
- **`st_gt_dashboard()`**: Generate professional FiveThirtyEight-styled dashboards with one line of code
- **`st_yoy_metrics()`**: **NEW!** Flexible year-over-year comparisons for any date range
- **`st_sales_report()`**: Platform-specific daily revenue and download data
- **`st_smart_metrics()`**: **NEW!** Fetch metrics with ID type detection and caching
- **`st_cache_info()`**: **NEW!** View app ID cache statistics
- **`st_clear_id_cache()`**: **NEW!** Clear the app ID cache

## ID Optimization & Caching

The package now includes intelligent ID caching to minimize API calls:

### Smart Metrics with ID Type Detection

**Important**: Users must specify exactly one type of ID at a time:
- Publisher ID
- Unified app ID (24-character hex)
- iOS app ID (numeric)
- Android app ID (package name)

```r
# Mix of iOS, Android, and unified IDs - ID types are detected and cached
metrics <- st_smart_metrics(
  app_ids = c(
    "553834731",                    # Candy Crush iOS
    "com.king.candycrushsaga",      # Candy Crush Android
    "5ba4585f539ce75b97db6bcb",     # Star Trek unified
    "1195621598",                   # Homescapes iOS
    "com.playrix.homescapes"        # Homescapes Android
  ),
  metrics = c("revenue", "downloads", "dau"),
  start_date = "2024-01-01",
  end_date = "2024-01-07"
)

# First call: Makes API calls to resolve IDs
# Subsequent calls: Uses cached IDs (30-day expiry)
```

### Cache Management
```r
# View cache statistics
st_cache_info()
# Output:
# App ID Cache Statistics:
#   Total entries: 25
#   Apps with iOS ID: 20
#   Apps with Android ID: 22
#   Apps with both platforms: 17
#   Average cache age: 5.2 days

# Clear cache if needed
st_clear_id_cache()

# Configure caching behavior
options(sensortowerR.use_cache = TRUE)        # Enable/disable caching
options(sensortowerR.cache_max_age_days = 30) # Cache expiry (days)
options(sensortowerR.verbose = TRUE)          # See optimization in action
```

### How It Works
1. **ID Type Detection**: Identifies iOS numeric, Android package, or unified hex IDs
2. **Cache First**: Checks local cache before making API calls
3. **Batch Lookups**: Resolves multiple IDs in a single API call when possible
4. **Persistent Storage**: Cache survives between R sessions
5. **Smart Grouping**: Groups compatible requests to minimize API calls

## Quick Examples

### Basic App Search
```r
# Search for apps
monopoly_info <- st_app_info("Monopoly Go")
pokemon_info <- st_app_info("Pokemon GO", limit = 1)
```

### App ID Lookup & Diagnostics
```r
# Resolve platform IDs from unified ID
app_ids <- st_app_lookup("5ba4585f539ce75b97db6bcb")  # Star Trek Fleet Command

# Use the resolved IDs with other functions
if (!is.null(app_ids)) {
  metrics <- st_yoy_metrics(
    os = "unified",
    ios_app_id = app_ids$ios_app_id,
    android_app_id = app_ids$android_app_id,
    years = c(2023, 2024, 2025),
    period_start = "01-01",
    period_end = "12-31",
    countries = "US",
    metrics = "revenue",
    granularity = "monthly"
  )
}

# Diagnose why an app ID isn't working
diagnosis <- st_api_diagnostics("5ba4585f539ce75b97db6bcb")
# Returns detailed analysis and recommendations
```

### ID Mapping & Resolution
```r
# Get complete ID mapping for apps
mapping <- st_get_unified_mapping(c("1427744264", "com.scopely.startrek"))

# View the mapping
mapping %>% 
  select(input_id, unified_app_id, ios_app_id, android_app_id, unified_app_name)

# Example output:
# input_id              unified_app_id           ios_app_id  android_app_id         unified_app_name
# 1427744264            5ba4585f539ce75b97db6bcb 1427744264  com.scopely.startrek   Star Trek Fleet Command
# com.scopely.startrek  5ba4585f539ce75b97db6bcb 1427744264  com.scopely.startrek   Star Trek Fleet Command

# Use mapping with other functions
for (i in 1:nrow(mapping)) {
  metrics <- st_yoy_metrics(
    os = "unified",
    ios_app_id = mapping$ios_app_id[i],
    android_app_id = mapping$android_app_id[i],
    years = c(2023, 2024, 2025),
    period_start = "01-01",
    period_end = "12-31",
    countries = c("US", "GB", "JP"),
    metrics = c("revenue", "downloads")
  )
}
```

### Batch Processing
```r
# Efficiently fetch metrics for multiple apps with mixed ID types
apps <- c(
  "553834731",                    # Candy Crush iOS
  "com.supercell.clashofclans",   # Clash of Clans Android
  "5ba4585f539ce75b97db6bcb"      # Star Trek hex ID
)

# Batch fetch with ID type detection (now supports DAU/WAU/MAU!)
batch_metrics <- st_batch_metrics(
  app_list = apps,
  metrics = c("revenue", "downloads"),
  date_range = list(start_date = "2025-01-01", end_date = "2025-06-30"),
  countries = "US",
  granularity = "monthly"
)

# Batch fetch with active user metrics
user_metrics_batch <- st_batch_metrics(
  app_list = apps,
  metrics = c("revenue", "downloads", "dau", "wau", "mau"),
  date_range = list(start_date = "2025-01-01", end_date = "2025-01-31"),
  countries = "US",
  granularity = "monthly",
  verbose = TRUE
)

# Or use year-to-date mode with all metrics
ytd_batch <- st_batch_metrics(
  app_list = apps,
  metrics = c("revenue", "downloads", "dau", "wau", "mau"),
  date_range = "ytd",
  countries = c("US", "GB", "JP"),
  granularity = "monthly"
)
```

### Publisher Apps
```r
# Get all Supercell games
supercell_apps <- st_publisher_apps("560c48b48ac350643900b82d")

# Fetch publisher-level revenue/downloads (unified sums iOS+Android)
publisher_batch <- st_batch_metrics(
  os = "unified",
  app_list = character(0),
  publisher_ids = c("560c48b48ac350643900b82d"),
  metrics = c("revenue", "downloads"),
  date_range = list(start_date = "2025-01-01", end_date = "2025-01-31"),
  countries = "US",
  granularity = "daily"
)
```

### App Metrics

The `st_metrics()` function now intelligently handles daily data by automatically using platform-specific endpoints when needed:

```r
# Simple usage - specify OS and app ID
metrics <- st_metrics(
  os = "ios",
  ios_app_id = "1195621598",  # Homescapes iOS
  countries = "US",  # Required parameter
  date_granularity = "daily",  # Required parameter
  start_date = Sys.Date() - 30,
  end_date = Sys.Date() - 1
)

# Best practice - use unified OS to get combined data
metrics <- st_metrics(
  os = "unified",
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes",
  countries = "US",  # Required parameter
  date_granularity = "daily",  # Required parameter
  start_date = Sys.Date() - 30,
  end_date = Sys.Date() - 1
)

# Advanced options
metrics <- st_metrics(
  os = "android",
  android_app_id = "com.king.candycrushsaga",
  countries = c("US", "GB"),   # Multiple countries
  date_granularity = "daily",  # Also supports weekly, monthly, quarterly
  start_date = Sys.Date() - 30,
  end_date = Sys.Date() - 1,
  verbose = TRUE               # Show progress messages
)

# The function automatically:
# - Detects if app_id is iOS (numeric) or Android (package name)
# - Uses platform-specific endpoints for daily data (unified endpoint returns empty)
# - Combines iOS and Android data for true unified metrics
# - Falls back gracefully when data is unavailable
```

### Category Rankings
```r
# Get official App Store rankings
ios_top_free <- st_category_rankings(
  os = "ios",
  category = 6014,  # Games
  chart_type = "topfreeapplications",
  country = "US",
  limit = 10
)

# Get Google Play top grossing games
android_top_grossing <- st_category_rankings(
  os = "android",
  category = "game",
  chart_type = "topgrossing",
  country = "US",
  limit = 20
)
```

### App Details
```r
# Get detailed metadata for specific apps
app_details <- st_app_details(
  app_ids = c("553834731", "1053012308"),  # Candy Crush, Clash Royale
  os = "ios"
)

# View key information
app_details %>%
  select(app_name, publisher_name, rating, price, description) %>%
  glimpse()
```

### Top Charts with Enhanced Metrics
```r
# Top apps by revenue (default measure)
top_revenue <- st_top_charts(category = 6000)  # iOS Games

# Top apps by downloads 
top_downloads <- st_top_charts(measure = "units", category = 6000)

# Top Role Playing games by MAU with rich analytics
top_rpg_games <- st_top_charts(
  measure = "MAU",
  category = 7014  # Role Playing category
)

# View enhanced custom metrics (40+ metrics available!)
top_rpg_games %>%
  select(unified_app_name, entities.users_absolute, 
         downloads_180d_ww, revenue_180d_ww, retention_1d_us, rpd_alltime_us) %>%
  head()
```

### Game Market Summary
```r
# Game market overview analysis
# iOS games market summary (last 7 days)
game_market <- st_game_summary(
  categories = 7001,           # Game category
  os = "ios",                  # iOS platform
  countries = c("US", "GB"),   # Multiple countries
  date_granularity = "daily",  # Daily breakdown
  start_date = Sys.Date() - 7
)

# Analyze market trends
# Note: iPhone and iPad data are automatically combined into iOS
market_trends <- game_market %>%
  group_by(Date) %>%
  summarise(
    Total_Revenue = sum(`iOS Revenue`, na.rm = TRUE),
    Total_Downloads = sum(`iOS Downloads`, na.rm = TRUE)
  )
```

### Top Publishers Analysis
```r
# Get top 10 game publishers by revenue (end_date auto-calculated)
top_publishers <- st_top_publishers(
  measure = "revenue",
  os = "unified",
  category = 6014,  # Games
  time_range = "month",
  date = "2025-01-01",  # Must be first day of month
  limit = 10
)
# Message: Auto-setting end_date to 2025-01-31 (last day of month starting 2025-01-01)

# View publisher rankings with revenue and date ranges
top_publishers %>%
  select(rank, publisher_name, date_start, date_end, revenue_usd) %>%
  head()
# Shows exact period covered: e.g., date_start: "2025-01-01", date_end: "2025-01-31"

# Get top publishers by downloads with growth metrics
growth_publishers <- st_top_publishers(
  measure = "units",
  comparison_attribute = "delta",
  time_range = "week",
  limit = 20
)
```

### Year-over-Year Comparisons (NEW!)

The new `st_yoy_metrics()` function makes it easy to compare any date range across multiple years:

```r
# Compare Q1 performance across years
 q1_comparison <- st_yoy_metrics(
  os = "ios",
  ios_app_id = "553834731",  # Candy Crush
  years = c(2022, 2023, 2024),
  period_start = "01-01",
  period_end = "03-31",
  countries = "US",
  metrics = c("revenue", "downloads", "dau"),
  granularity = "daily"
)

# The function automatically calculates YoY changes
q1_comparison %>%
  filter(metric == "revenue", !is.na(yoy_change)) %>%
  select(year, value, yoy_change, yoy_change_absolute)

# Compare holiday season performance
 holiday_yoy <- st_yoy_metrics(
  os = "unified",
  unified_app_id = "5ba4585f539ce75b97db6bcb",
  years = c(2021, 2022, 2023),
  period_start = "11-01",
  period_end = "12-31",
  countries = c("US", "GB", "JP"),
  metrics = c("revenue", "downloads"),
  granularity = "monthly"
)

# Use Date objects for precise periods
 valentine_campaign <- st_yoy_metrics(
  os = "android",
  android_app_id = "com.king.candycrushsaga",
  years = NULL,  # Defaults to current and previous year
  period_start = as.Date("2024-02-14"),
  period_end = as.Date("2024-02-28"),
  countries = c("US", "GB", "DE"),
  metrics = c("revenue", "downloads", "dau"),
  granularity = "daily"
)

# Calculate growth from baseline year
growth_analysis <- calculate_yoy_growth(q1_comparison, baseline_year = 2022)

# View growth index (2022 = 100)
growth_analysis %>%
  filter(metric == "revenue") %>%
  select(year, value, growth_index, growth_from_baseline)
```

Key features:
- **Flexible periods**: Any date range, not just full years
- **Smart date handling**: Automatically adjusts for leap years
- **YoY calculations**: Both percentage and absolute changes included
- **Growth analysis**: Helper function for baseline comparisons
- **All metrics supported**: Revenue, downloads, DAU, WAU, MAU

## NEW: Professional Dashboard Generation

Create stunning FiveThirtyEight-styled dashboards with a single line of code:

```r
# Get top charts data
top_games <- st_top_charts(category = 7012)  # Casino games

# Generate dashboard with ONE line!
st_gt_dashboard(top_games)

# Customize the dashboard
st_gt_dashboard(
  top_games,
  title = "Top Casino Games - US Market",
  ranking_metric = "revenue_30d_ww",
  save_path = "casino_dashboard.png"
)

# Minimal styling for reports
st_gt_dashboard(top_games, raw = TRUE)
```

### Dashboard Features
- **Automatic ranking** by customizable metric
- **Bar charts** for revenue, downloads, and engagement metrics
- **Retention heatmaps** with color gradients
- **Country flag emojis** for international markets
- **Gender demographics** with visual breakdown
- **Smart formatting** with 2 decimal places for billions
- **FiveThirtyEight styling** or clean minimal mode

### Dashboard Parameters
- `title`: Custom title for the dashboard
- `subtitle`: Auto-generated or custom subtitle
- `ranking_metric`: Choose which metric to rank by
- `show_demographics`: Toggle demographic columns
- `show_engagement`: Toggle DAU/WAU/MAU columns
- `show_retention`: Toggle retention metrics
- `retention_region`: Choose retention region ("us", "ww")
- `bar_charts`: Enable/disable bar chart visualizations
- `heatmap_retention`: Enable/disable retention heatmaps
- `raw`: Return minimal styling without visualizations
- `save_path`: Save directly to PNG file
- `color_scheme`: Customize colors for different metric types

## Unified Top Charts Function

The `st_top_charts()` function combines revenue, downloads, and active user metrics in one simple interface:

```r
# All these use the same function with different measures:
st_top_charts(measure = "revenue", category = 6000)    # Default
st_top_charts(measure = "units", category = 6000)      # Downloads  

# Note: When using custom filters with DAU sorting, use measure = "revenue"
# The custom filter will handle the actual DAU sorting
st_top_charts(
  measure = "revenue",  # Use revenue endpoint
  custom_fields_filter_id = "your_filter_id",
  category = 7014
)
```

## Enhanced Custom Metrics

The `st_top_charts()` function extracts comprehensive custom metrics:

- **Downloads**: `downloads_180d_ww`, `downloads_90d_us`, `downloads_30d_ww`, `downloads_alltime_ww`
- **Revenue**: `revenue_180d_ww`, `revenue_90d_us`, `revenue_30d_ww`, `revenue_alltime_ww`
- **Active Users**: `dau_30d_ww`, `wau_4w_ww`, `mau_month_ww`
- **Retention**: `retention_1d_us`, `retention_7d_us`, `retention_30d_us`, `retention_60d_us`
- **Monetization**: `rpd_alltime_us`, `arpu_90d_us`
- **Demographics**: `age_ww`, `male_share_us`, `female_share_us`
- **Platform**: `ios_share_ww`, `android_share_ww`

## Automatic Features

### App Name Lookup
For sales data (revenue/downloads), the package **automatically looks up app names** since the sales endpoint only provides app IDs:

```r
# Sales data automatically gets app names resolved
top_revenue <- st_top_charts(measure = "revenue", category = 6000)
# Returns: "Pokémon GO" instead of just "834731712"
```

### Intelligent Data Type Handling
The new functions ensure consistent data types for seamless joining:

```r
# Rankings return character app_ids, details also convert to character
# This allows direct joining without type conversion
rankings %>% left_join(details, by = "app_id")  # Works seamlessly
```

### App Deduplication
By default, apps with the same name but different platform/regional SKUs are consolidated:

```r
# Genshin Impact (8 different app IDs) becomes one row with aggregated metrics
top_games <- st_top_charts(category = 7014)
# Downloads/revenue are summed, rates/percentages are averaged
```

## Combining Functions for Enriched Data

The package is designed with composability in mind. Here's how to combine functions for richer insights:

### Enriching Rankings with App Details
```r
# Step 1: Get current rankings
rankings <- st_category_rankings(
  os = "ios",
  category = 6014,  # Games
  chart_type = "topgrossingapplications",
  limit = 10
)

# Step 2: Get detailed metadata for ranked apps
details <- st_app_details(
  app_ids = rankings$app_id,
  os = "ios"
)

# Step 3: Combine for enriched rankings
enriched_rankings <- rankings %>%
  left_join(details %>% 
    select(app_id, app_name, publisher_name, rating, 
           rating_count, release_date), 
    by = "app_id") %>%
  arrange(rank)

# View top grossing games with details
enriched_rankings %>%
  select(rank, app_name, publisher_name, rating, rating_count) %>%
  head(10)
```

### Complete Market Analysis Workflow
```r
# 1. Find a category from a known app
app_info <- st_app_info("Genshin Impact", return_all_fields = TRUE, limit = 1)
rpg_category <- app_info %>%
  tidyr::unnest(category_details) %>%
  filter(grepl("Role Playing", category_name)) %>%
  pull(category_id) %>%
  first()

# 2. Get current store rankings
current_rankings <- st_category_rankings(
  os = "ios",
  category = rpg_category,
  chart_type = "topgrossingapplications",
  limit = 20
)

# 3. Get performance metrics for top apps
top_metrics <- st_top_charts(
  measure = "revenue",
  category = rpg_category,
  regions = "US"
)

# 4. Get detailed app information
app_details <- st_app_details(
  app_ids = current_rankings$app_id[1:10],
  os = "ios"
)

# 5. Combine all data and create dashboard
combined_data <- current_rankings %>%
  left_join(app_details %>% select(app_id, app_name, publisher_name), by = "app_id") %>%
  left_join(top_metrics %>% select(app_id, revenue_30d_ww, mau_month_ww), by = "app_id")

# Generate professional dashboard
st_gt_dashboard(
  top_metrics,
  title = "RPG Games Market Analysis",
  save_path = "rpg_market_dashboard.png"
)
```

## Example Workflow: Social Casino Analysis

```r
# Load packages
library(sensortowerR)

# Find Social Casino category from a specific game
monopoly_info <- st_app_info("Monopoly Go", return_all_fields = TRUE, limit = 1)
social_casino_id <- monopoly_info %>%
  tidyr::unnest(category_details) %>%
  filter(grepl("Casino", category_name)) %>%
  pull(category_id) %>%
  first()

# Get top social casino games (US focus)
top_casino <- st_top_charts(
  measure = "revenue",
  category = social_casino_id,
  regions = "US"
)

# Create professional dashboard with one line
st_gt_dashboard(
  top_casino,
  title = "Top Social Casino Games - US Market",
  save_path = "social_casino_dashboard.png"
)
```

### Example Output: Social Casino Dashboard

The `social_casino_analysis.R` example generates a comprehensive analytics dashboard for top social casino games:

<p align="center"><img src="inst/images/social_casino_dashboard.png" width="100%"></p>

This dashboard showcases:
- **180-day Revenue Performance** - ranking metric with bar charts
- **User Engagement** - DAU, WAU, MAU with visual comparisons
- **Downloads Metrics** - 30-day, 180-day, and lifetime totals
- **Retention Rates** - Day 1, 7, 30, and 60 retention with heatmaps
- **Monetization** - Revenue Per Download (RPD) efficiency
- **Demographics** - Age and gender breakdowns

Additional visualizations generated:
- Revenue trend analysis
- DAU/MAU engagement ratios
- Retention curves comparison
- Monetization efficiency metrics

Run the full example with:
```r
source("examples/category_analyses/social_casino_analysis.R")
```

## Data Cleaning and Processing

The package automatically handles:
- **Numeric formatting**: Removes special characters ($, %, commas) from numeric values
- **Date parsing**: Converts date strings to proper Date objects
- **Missing values**: Gracefully handles NA values
- **Retention values**: Maintains percentage format (0-100)
- **Multi-platform aggregation**: Intelligently combines metrics across platforms

## Defaults

The package defaults to simplify usage:

- **OS**: `"unified"` (combines iOS + Android)
- **Region**: `"WW"` (Worldwide) 
- **Date**: Current month start to present
- **Limit**: `20` results
- **Deduplication**: `TRUE` (consolidates apps with same name)

## Examples Directory Structure

```
examples/
├── category_analyses/         # Comprehensive category analysis scripts
│   ├── social_casino_analysis.R
│   └── squad_rpg_analysis_*.R
├── dashboard_examples/        # Dashboard generation examples
│   └── test_gt_dashboard.R
└── simple_examples/          # Basic usage examples
    ├── api_optimization_demo.R
    ├── game_summary_demo.R
    └── simple_dashboard_example.R
```

## Advanced Features

### Custom Metric Extraction
The package extracts and renames 40+ custom metrics from Sensor Tower's aggregate and entity tags, making them easily accessible with intuitive column names.

### Smart Data Processing
- Automatic deduplication of apps with multiple SKUs
- Intelligent metric aggregation (sum for additive metrics, average for rates)
- Proper handling of user metrics (DAU/MAU/WAU averaged, not summed)

### Professional Visualization
- FiveThirtyEight-quality table styling
- Automatic bar chart generation
- Retention heatmaps with gradient coloring
- Country flag emoji support
- Gender demographic visualization

## Recent Changes

### Version 0.2.0 (2025-01-27) 
- **Major improvement**: Smart API lag detection for current periods
- `st_top_publishers()` now automatically detects actual data availability
- For current month/quarter/year, `date_end` reflects the latest available data (not theoretical month-end)
- Example: In July 2025, returns data through July 26 (not July 31) when API has 1-day lag
- Prevents overestimating periods and ensures accurate date ranges

### Version 0.1.9 (2025-01-27)
- **Important**: Added `date_start` and `date_end` columns to `st_top_publishers()` output
- These columns clarify the exact period covered by the data, regardless of time_range grain
- Essential for understanding data scope when using week/month/quarter/year aggregations
- Prevents confusion about what dates are actually included in the results

### Version 0.1.8 (2025-01-27)
- **Major Update**: Unified `st_metrics()` function now intelligently handles daily data limitations
- Automatically switches to platform-specific endpoints when daily data is requested
- Combines iOS and Android data for true unified metrics
- Added comprehensive endpoint testing documentation
- Updated README with API limitation matrix

### Version 0.5.0 (2025-01-26)
- Enhanced cross-platform testing with rhub v2 GitHub Actions
- Improved CRAN submission readiness with stricter checks
- Updated documentation for function examples
- Added comprehensive cross-platform test suite
- Fixed minor issues identified during CRAN pre-submission checks

## API Endpoint Reference

### Custom Field Filter Support

Custom field filters allow you to use predefined filters from the Sensor Tower web interface:

| Function | Supports custom_fields_filter_id | Notes |
|----------|----------------------------------|-------|
| `st_sales_report()` | ✅ | Full support for custom filters |
| `st_category_rankings()` | ✅ | Category parameter becomes optional |
| `st_top_charts()` | ✅ | Uses comparison attributes endpoint |
| `st_metrics()` | ✅ | When using unified endpoint |
| `st_top_publishers()` | ❌ | Does not support custom filters |

```r
# Example: Use a custom filter for category rankings
rankings <- st_category_rankings(
  os = "unified",
  custom_fields_filter_id = "67890abcdef1234567890abc",
  custom_tags_mode = "include_unified_apps",
  country = "US",
  date = "2024-01-01"
)
```

**Important Notes**:

- `custom_fields_filter_id`: 24-character hexadecimal string from Sensor Tower web interface
- `custom_tags_mode`: Required for unified OS (`"include"`, `"exclude"`, or `"include_unified_apps"`)

### Endpoint Capabilities

| Function | Unified OS | Daily Data | Weekly | Monthly | Notes |
|----------|------------|------------|--------|---------|-------|
| `st_metrics()` | ✅ | ✅ | ✅ | ✅ | Automatically combines iOS & Android |
| `st_sales_report()` | ❌ | ✅ | ✅ | ✅ | Platform-specific only |
| `st_batch_metrics()` | ❌ | ✅ | ✅ | ✅ | Use platform-specific OS |

### App ID Format Guide

| Platform | Format | Example | Description |
|----------|--------|---------|-------------|
| iOS | Numeric string | `"1195621598"` | Apple App Store ID |
| Android | Package name | `"com.playrix.homescapes"` | Google Play package identifier |
| Unified | 24-char hex | `"5ba4585f539ce75b97db6bcb"` | Sensor Tower's internal ID |

### App ID Parameters (v0.8.0+)

**st_sales_report()** now uses specific parameters:

```r
# iOS data
ios_data <- st_sales_report(
  os = "ios",
  ios_app_id = "1195621598",  # iOS numeric ID
  countries = "US",
  date_granularity = "daily",
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)

# Android data
android_data <- st_sales_report(
  os = "android",
  android_app_id = "com.playrix.homescapes",  # Android package
  countries = "US",
  date_granularity = "daily",
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)
```

**st_metrics()** for unified data:

```r
# Unified data - combines iOS and Android
unified_data <- st_metrics(
  os = "unified",
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes",
  countries = "US",
  date_granularity = "daily",
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)
```

### Automatic ID Resolution

The package intelligently handles cross-platform IDs:

- Providing `unified_app_id` with any OS automatically looks up the correct platform ID
- Providing `android_app_id` with `os="ios"` will attempt to find the iOS version
- Clear error messages if the app doesn't exist on the requested platform

```r
# Convert unified ID to platform-specific IDs
app_info <- st_app_lookup("5ba4585f539ce75b97db6bcb")
# Returns: list(ios_app_id = "1195621598", 
#               android_app_id = "com.playrix.homescapes",
#               app_name = "Homescapes",
#               publisher_name = "Playrix")
```

### Key Limitations

- **st_sales_report()** does not support `os="unified"` - use platform-specific calls
- **st_batch_metrics()** does not support `os="unified"`
- For unified data, use `st_metrics()` which works perfectly with all date granularities
- The API automatically segments long date ranges to prevent timeouts

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This package is licensed under the MIT License.
