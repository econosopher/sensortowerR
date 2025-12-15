# sensortowerR

<p align="center"><img src="inst/images/sensortowerR_sticker.png" width="200"></p>

An R package for interfacing with the Sensor Tower API to fetch mobile app analytics data, including app info, publisher details, revenue/download estimates, active user metrics, and professional dashboard generation.

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

## Quick Start

### Find Apps and Get Data

```r
library(sensortowerR)
library(dplyr)

# Step 1: Search for an app by name
apps <- st_app_info("Royal Match")
unified_id <- apps$unified_app_id[1]

# Step 2: Get revenue/downloads
revenue <- st_sales_report(
  os = "ios",
  unified_app_id = unified_id,
  countries = "US",
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  date_granularity = "monthly"
)

# Step 3: Get active users (MAU/DAU)
users <- st_batch_metrics(
  os = "unified",
  app_list = unified_id,
  metrics = c("mau", "dau"),
  date_range = list(start_date = "2024-01-01", end_date = "2024-12-31"),
  countries = "US",
  granularity = "monthly"
)
```

### Publisher Portfolio Analysis

Get comprehensive portfolio data with one function call:

```r
# Simple usage
portfolio <- st_publisher_portfolio("Lilith Games")

# Piped workflow
"Supercell" %>%
  st_publisher_portfolio(metrics = c("revenue", "mau")) %>%
  filter(revenue_2024 > 10000000)
```

### Top Charts and Market Analysis

```r
# Top games by revenue
top_games <- st_top_charts(
  measure = "revenue",
  os = "unified",
  category = 6014,  # Games
  regions = "US",
  time_range = "month"
)

# Generate a professional dashboard
st_gt_dashboard(top_games, title = "Top Games - US Market")
```

## Core Functions

| Function | Purpose |
|----------|---------|
| `st_app_info()` | Search for apps by name |
| `st_app_lookup()` | Resolve platform IDs from unified IDs |
| `st_sales_report()` | Platform-specific revenue and downloads |
| `st_unified_sales_report()` | Unified revenue with multi-regional SKU aggregation |
| `st_batch_metrics()` | Efficient metrics for multiple apps (MAU/DAU/WAU) |
| `st_publisher_portfolio()` | Complete publisher analysis in one call |
| `st_top_charts()` | Top apps by revenue, downloads, or active users |
| `st_top_publishers()` | Publisher rankings |
| `st_category_rankings()` | Official app store rankings |
| `st_yoy_metrics()` | Year-over-year comparisons |
| `st_gt_dashboard()` | Professional FiveThirtyEight-styled dashboards |

## Key Workflows

### Finding Specific Apps (DO THIS)

```r
# 1. Search by name
apps <- st_app_info("Candy Crush")

# 2. Get platform IDs
ids <- st_app_lookup(apps$unified_app_id[1])

# 3. Fetch data
data <- st_sales_report(os = "ios", ios_app_id = ids$ios_app_id, ...)
```

### DON'T: Use Top Charts to Find Specific Apps

```r
# BAD - Apps may not be in top N, wastes API calls
top <- st_top_charts(limit = 1000)
my_apps <- filter(top, name %in% c("My App"))  # May return empty!
```

## Data Availability

| Data Type | Function | Time-Series? | Region Coverage |
|-----------|----------|--------------|-----------------|
| Revenue/Downloads (unified) | `st_unified_sales_report()` | Yes | All countries |
| Revenue/Downloads (platform) | `st_sales_report()` | Yes | All countries |
| MAU/DAU/WAU | `st_batch_metrics()` | Yes | All countries |
| Retention (D1-D60) | `st_app_enriched()` | Snapshot only | US, WW |
| Demographics | `st_app_enriched()` | Snapshot only | US only |

## Multi-Regional SKU Aggregation

Some games have multiple regional publishers. Use `st_unified_sales_report()` to aggregate all SKUs:

```r
# Gets TRUE unified revenue across all regional versions
sales <- st_unified_sales_report(
  unified_app_id = "67ec0bf3e540b65904256cc4",
  countries = "WW",
  start_date = "2024-01-01",
  end_date = "2024-12-31"
)
```

## Category Codes

Common categories for use with API functions:

**iOS**: `6014` (Games), `7014` (RPG), `7012` (Casino), `7001` (Action)

**Android**: `GAME`, `game_role_playing`, `game_casino`, `game_action`

Use `st_categories()` for a complete list.

## API Limitations

- **Daily data**: Use platform-specific endpoints (iOS/Android), not unified
- **Unified sales endpoint**: Returns empty for all granularities - use `st_metrics()` which handles this automatically
- **Active users endpoint**: Does not support unified OS - `st_batch_metrics()` handles this

## ID Caching

The package caches app ID mappings to reduce API calls:

```r
# View cache statistics
st_cache_info()

# Save cache to persist between sessions
save_id_cache()

# Load previously saved cache
load_id_cache()

# Clear cache
st_clear_id_cache()
```

Cache is stored in the CRAN-compliant location: `tools::R_user_dir("sensortowerR", "cache")`

## Changelog

See [NEWS.md](https://github.com/econosopher/sensortowerR/blob/main/NEWS.md) for version history and changes.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This package is licensed under the MIT License.
