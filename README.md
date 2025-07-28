# sensortowerR

<p align="center"><img src="inst/images/sensortowerR_sticker.png" width="200"></p>

An R package for interfacing with the Sensor Tower API to fetch mobile app analytics data, including app info, publisher details, revenue/download estimates, active user metrics, and professional dashboard generation.

## What's New

### v0.3.0
- **YTD Calculation Fix**: Added `calculate_ytd_change()` function for accurate year-to-date metrics
- **Important**: The API's `time_range = "year"` with delta calculations may not represent simple YTD summation
- **Warnings**: Added console warnings when using year comparisons that might be misleading
- **New columns**: Added `ytd_warning` column to flag when metrics need recalculation

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

1. **Unified endpoints return empty responses for daily data**
   - The unified sales_report_estimates endpoint does not support daily granularity
   - Platform-specific endpoints (iOS/Android) must be used instead
   - The `st_metrics()` function automatically handles this limitation

2. **Working Endpoint Matrix** (as of January 2025):
   ```
   ✅ iOS + daily data = SUCCESS
   ✅ Android + daily data = SUCCESS  
   ✅ iOS + monthly/weekly/quarterly = SUCCESS
   ✅ Android + monthly/weekly/quarterly = SUCCESS
   ❌ Unified + daily data = EMPTY RESPONSE
   ✅ Unified + monthly/weekly/quarterly = SUCCESS (limited endpoints)
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

**Solution**: Use the new `calculate_ytd_change()` function for accurate YTD metrics:

```r
# WRONG - May give misleading YTD changes
ytd_data <- st_top_publishers(
  time_range = "year",
  comparison_attribute = "delta",  # This won't be accurate YTD!
  date = "2025-01-01"
)

# CORRECT - Accurate YTD calculation
ytd_changes <- calculate_ytd_change(
  publisher_ids = c("pub1", "pub2"),
  current_year = 2025,
  measure = "revenue",
  country = "US",
  end_month = 7  # For Jan-Jul YTD
)
```

The package now:
- Shows warnings when using potentially misleading year comparisons
- Adds `ytd_warning` column to flag these metrics
- Provides `calculate_ytd_change()` for accurate monthly-based YTD calculations

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
- **`st_publisher_apps()`**: Get all apps from a specific publisher  
- **`st_metrics()`**: Detailed daily metrics for specific apps (see note below)
- **`st_top_charts()`**: Unified function for all top charts (revenue, downloads, DAU, WAU, MAU)
- **`st_game_summary()`**: Game market summary (aggregated downloads/revenue by categories and countries)
- **`st_category_rankings()`**: **NEW!** Get official app store rankings by category
- **`st_app_details()`**: **NEW!** Fetch comprehensive app metadata and store listings
- **`st_top_publishers()`**: **NEW!** Get top publishers by revenue or downloads
- **`st_publisher_category_breakdown()`**: **NEW!** Analyze publisher revenue across categories
- **`st_gt_dashboard()`**: Generate professional FiveThirtyEight-styled dashboards with one line of code
- **`st_sales_report()`**: Platform-specific daily revenue and download data

## Quick Examples

### Basic App Search
```r
# Search for apps
monopoly_info <- st_app_info("Monopoly Go")
pokemon_info <- st_app_info("Pokemon GO", limit = 1)
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