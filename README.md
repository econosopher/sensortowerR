# sensortowerR

<p align="center"><img src="inst/images/sensortowerR_sticker.png" width="200"></p>

An R package for interfacing with the Sensor Tower API to fetch mobile app analytics data, including app info, publisher details, revenue/download estimates, active user metrics, and professional dashboard generation.

## What's New

### v0.4.2
- **New Function**: `st_get_unified_mapping()` - Get complete ID mapping between platforms
  - Retrieves the mapping between platform-specific and unified app IDs
  - Uses the comparison attributes endpoint for rich entity data
  - Falls back to `st_app_lookup()` when needed
  - Returns unified_app_id, ios_app_id, android_app_id, and publisher info
- **Enhanced ID Tracking**: Functions now preserve original input IDs
  - `st_ytd_metrics()` adds `original_unified_id` column for easier joins
  - `st_batch_metrics()` maintains `original_id` throughout processing
  - Helps with downstream analysis when using unified IDs
- **Improved Fallback Logic**: `st_batch_metrics()` now has smarter fallbacks
  - Automatically detects when unified IDs return zero revenue
  - Attempts to resolve and retry with platform-specific IDs
  - Shows warnings in verbose mode to help diagnose issues

### v0.4.1
- **Bug Fix**: Fixed `st_ytd_metrics()` error when data columns were missing
  - Now ensures required columns exist before aggregation
  - Handles empty data responses gracefully
- **Automatic Fallback**: When unified app IDs return zero data, attempts fallback to platform-specific IDs
  - Automatically detects Sensor Tower hex IDs and resolves platform-specific IDs
  - Works transparently - no code changes needed
- **New Function**: `st_app_lookup()` - Resolve platform IDs from unified IDs
  - Accepts Sensor Tower hex IDs, iOS numeric IDs, or Android package names
  - Returns both iOS and Android IDs for use with other API functions
  - Helpful when unified endpoints don't work with certain ID types
- **New Function**: `st_api_diagnostics()` - Diagnose API issues systematically
  - Detects ID type and tests various endpoints
  - Provides specific recommendations for each app
  - Helps debug why certain IDs aren't working
- **New Function**: `st_batch_metrics()` - Efficiently fetch metrics for multiple apps
  - Automatically resolves mixed ID types (iOS, Android, hex IDs)
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

### v0.3.5
- **MAU Support Added**: `st_ytd_metrics()` now supports Monthly Active Users (MAU) metrics
- **Average MAU Calculation**: MAU is calculated as average monthly users for YoY comparisons
- **Complete Engagement Metrics**: DAU/WAU/MAU ratios enable comprehensive engagement analysis
- **ARPMAU Calculation**: Combine MAU with revenue for monetization insights

### v0.3.4
- **WAU Support Added**: `st_ytd_metrics()` now supports Weekly Active Users (WAU) metrics
- **Average WAU Calculation**: WAU is calculated as average weekly users for fair comparisons
- **DAU/WAU Ratio Analysis**: Easily calculate daily engagement rates from active user metrics
- **Complete Active Users**: Supports both DAU and WAU for comprehensive user analysis

### v0.3.3
- **DAU Support in YTD Metrics**: `st_ytd_metrics()` now supports Daily Active Users (DAU) metrics
- **Average DAU Calculation**: DAU is calculated as average daily users for meaningful YoY comparisons
- **Intelligent DAU Batching**: Fetches full YTD DAU data efficiently with minimal API calls
- **Platform-Specific DAU**: Automatically handles iOS (iPhone + iPad) and Android user counts
- **Unified Endpoint Fix**: `st_metrics()` now uses fallback pattern for broken unified endpoint

### v0.3.1
- **New YTD Metrics Function**: `st_ytd_metrics()` fetches year-to-date metrics across multiple years
- **Multi-Year Support**: Compare metrics across multiple years with a single function call
- **Flexible Periods**: Default to YTD through last completed week, or specify custom date ranges
- **Entity Flexibility**: Works with both individual apps and publishers
- **Tidy Output**: Returns data in long format for easy analysis and visualization

### v0.3.0
- **YTD Metrics Function**: Added `st_ytd_metrics()` for accurate year-to-date metrics
- **Important**: The API's `time_range = "year"` with delta calculations may not represent simple YTD summation
- **Smart Batching**: Intelligently fetches data in optimal chunks to minimize API calls
- **Caching Support**: Built-in 24-hour caching to avoid redundant API calls

### v0.2.2
- **Country support for category breakdown**: `st_publisher_category_breakdown()` now accepts a `country` parameter, allowing you to get category breakdowns for specific markets

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
   # Fetch data for multiple apps at once
   revenue_data <- st_sales_report(
     app_ids = c("1195621598", "553834731", "1053012308"),  # Multiple iOS apps
     countries = "US",
     start_date = Sys.Date() - 30,
     end_date = Sys.Date() - 1,
     date_granularity = "daily"
   )
   
   # This makes 1 API call instead of 3!
   ```

3. **Performance Benefits**:
   - Reduces API calls by up to 90% for multi-app analyses
   - Faster execution time
   - Lower risk of hitting rate limits
   - More efficient data processing

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

### Year-to-Date (YTD) Calculations - IMPORTANT

**Critical Issue**: The API's `time_range = "year"` with comparison attributes (delta/transformed_delta) does NOT return simple YTD summation. It may include:
- Annualized projections
- Different calculation methods
- Full year estimates

**Solution**: Use the `st_ytd_metrics()` function for accurate YTD metrics:

```r
# WRONG - May give misleading YTD changes
ytd_data <- st_top_publishers(
  time_range = "year",
  comparison_attribute = "delta",  # This won't be accurate YTD!
  date = "2025-01-01"
)

# CORRECT - Use the new st_ytd_metrics() function
ytd_metrics <- st_ytd_metrics(
  unified_app_id = "553834731",  # Candy Crush
  years = c(2023, 2024, 2025),  # Multiple years
  cache_dir = ".cache/ytd"  # Optional: enable caching
)

# The function returns tidy data
# Columns: entity_id, entity_name, entity_type, year, date_start, date_end, country, metric, value

# Calculate growth yourself
ytd_metrics %>%
  filter(metric == "revenue") %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(yoy_growth = (`2024` - `2023`) / `2023` * 100)
```

Key features of `st_ytd_metrics()`:
- **Multi-year support**: Fetch metrics for multiple years in one call
- **Smart defaults**: YTD through last completed week (ending Saturday)
- **Custom periods**: Specify any date range (e.g., "02-01" to "02-28")
- **Automatic caching**: Reuses data across years to minimize API calls
- **Works with publishers**: Use `publisher_id` instead of app IDs
- **DAU/WAU/MAU Support**: Fetch average Daily/Weekly/Monthly Active Users with intelligent batching
- **Platform-aware**: Automatically handles iOS (iPhone + iPad) and Android users
- **YoY Comparable**: Average DAU/WAU/MAU enables fair comparisons across different period lengths

The package now:
- Provides `st_ytd_metrics()` for accurate YTD calculations
- Fetches data in optimal batches (monthly chunks where possible)
- Supports multiple entities and years in a single function call
- Returns data in tidy format for easy analysis

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

## Core Functions

- **`st_app_info()`**: Search for apps and get basic information
- **`st_app_lookup()`**: **NEW!** Resolve platform-specific IDs from unified IDs
- **`st_api_diagnostics()`**: **NEW!** Diagnose why app IDs aren't working and get recommendations
- **`st_batch_metrics()`**: **NEW!** Efficiently fetch metrics for multiple apps with mixed ID types
- **`st_publisher_apps()`**: Get all apps from a specific publisher  
- **`st_metrics()`**: Detailed daily metrics for specific apps (see note below)
- **`st_top_charts()`**: Unified function for all top charts (revenue, downloads, DAU, WAU, MAU)
- **`st_game_summary()`**: Game market summary (aggregated downloads/revenue by categories and countries)
- **`st_category_rankings()`**: **NEW!** Get official app store rankings by category
- **`st_app_details()`**: **NEW!** Fetch comprehensive app metadata and store listings
- **`st_top_publishers()`**: **NEW!** Get top publishers by revenue or downloads
- **`st_publisher_category_breakdown()`**: **NEW!** Analyze publisher revenue across categories
- **`st_ytd_metrics()`**: **NEW!** Fetch year-to-date metrics (revenue, downloads, DAU, WAU, MAU) across multiple years
- **`st_gt_dashboard()`**: Generate professional FiveThirtyEight-styled dashboards with one line of code
- **`st_sales_report()`**: Platform-specific daily revenue and download data

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
  metrics <- st_ytd_metrics(
    ios_app_id = app_ids$ios_app_id,
    android_app_id = app_ids$android_app_id,
    years = 2025,
    metrics = "revenue",
    countries = "WW"
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
  metrics <- st_ytd_metrics(
    ios_app_id = mapping$ios_app_id[i],
    android_app_id = mapping$android_app_id[i],
    years = 2025,
    metrics = c("revenue", "downloads"),
    countries = "WW"
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

# Batch fetch with automatic ID resolution (now supports DAU/WAU/MAU!)
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
  countries = "WW"
)
```

### Publisher Apps
```r
# Get all Supercell games
supercell_apps <- st_publisher_apps("560c48b48ac350643900b82d")
```

### App Metrics

The `st_metrics()` function now intelligently handles daily data by automatically using platform-specific endpoints when needed:

```r
# Simple usage - auto-detects platform and fetches data
metrics <- st_metrics(
  app_id = "1195621598",  # Homescapes iOS
  start_date = Sys.Date() - 30,
  end_date = Sys.Date() - 1
)

# Best practice - provide both platform IDs for complete data
metrics <- st_metrics(
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes",
  start_date = Sys.Date() - 30,
  end_date = Sys.Date() - 1
)

# Advanced options
metrics <- st_metrics(
  app_id = "com.king.candycrushsaga",
  combine_platforms = FALSE,  # Keep iOS and Android data separate
  date_granularity = "daily",  # Also supports weekly, monthly, quarterly
  countries = c("US", "GB"),   # Multiple countries
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

# Analyze publisher category breakdown
publisher_ids <- top_publishers$publisher_id[1:5]
category_breakdown <- st_publisher_category_breakdown(
  publisher_ids = publisher_ids,
  time_range = "month"
)

# View revenue distribution by category
category_breakdown %>%
  group_by(publisher_name) %>%
  arrange(desc(category_percentage)) %>%
  slice_head(n = 3)  # Top 3 categories per publisher
```

### Year-to-Date Metrics (Now with DAU, WAU, and MAU!)

**Important**: For most accurate results, use platform-specific IDs instead of unified app IDs:

```r
# RECOMMENDED: Use platform-specific IDs for accurate data
ytd_metrics <- st_ytd_metrics(
  ios_app_id = c("553834731", "1195621598"),     # iOS app IDs
  android_app_id = c("com.king.candycrushsaga", "com.playrix.homescapes"),  # Android package names
  years = c(2023, 2024, 2025),
  metrics = c("revenue", "downloads", "dau", "wau", "mau"),
  countries = "WW",  # Required parameter
  cache_dir = ".cache/ytd"  # Enable caching
)

# Alternative: Unified app IDs (may return zero for some apps)
ytd_metrics <- st_ytd_metrics(
  unified_app_id = c("553834731", "1195621598"),  # Candy Crush, Homescapes
  years = c(2023, 2024, 2025),
  metrics = c("revenue", "downloads", "dau", "wau", "mau"),  # Full active user support!
  countries = "WW",  # Required parameter
  cache_dir = ".cache/ytd"  # Enable caching
)

# The function returns tidy data perfect for analysis
ytd_metrics %>%
  filter(metric == "revenue") %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(
    yoy_growth_2024 = (`2024` - `2023`) / `2023` * 100,
    yoy_growth_2025 = (`2025` - `2024`) / `2024` * 100
  )

# Analyze DAU trends (values are already averaged)
dau_summary <- ytd_metrics %>%
  filter(metric == "dau") %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(
    yoy_change = (`2025` - `2024`) / `2024` * 100
  )

# Calculate engagement ratios (DAU/WAU/MAU)
engagement <- ytd_metrics %>%
  filter(metric %in% c("dau", "wau", "mau"), year == 2025) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(
    dau_mau_ratio = dau / mau,
    wau_mau_ratio = wau / mau,
    daily_engagement_pct = dau_mau_ratio * 100,
    weekly_engagement_pct = wau_mau_ratio * 100
  )

# Custom date ranges (e.g., Q1 comparison)
q1_metrics <- st_ytd_metrics(
  unified_app_id = "1053012308",  # MONOPOLY GO!
  years = c(2023, 2024, 2025),
  period_start = "01-01",
  period_end = "03-31",
  metrics = c("revenue", "downloads", "dau")
)

# Active users for multiple apps with platform-specific IDs
multi_app_users <- st_ytd_metrics(
  ios_app_id = c("553834731", "1195621598"),
  android_app_id = c("com.king.candycrushsaga", "com.playrix.homescapes"),
  years = 2025,
  metrics = c("dau", "mau")  # DAU and MAU for engagement analysis
)

# Works with publishers too (but not for active users)
publisher_ytd <- st_ytd_metrics(
  publisher_id = c("pub123", "pub456"),
  years = 2025,
  metrics = c("revenue", "downloads")  # DAU/WAU/MAU not available for publishers
)
```

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
st_top_charts(measure = "MAU", category = 7014)        # Monthly Active Users
st_top_charts(measure = "DAU", category = 7014)        # Daily Active Users
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

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This package is licensed under the MIT License.