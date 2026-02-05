# sensortowerR

<p align="center"><img src="inst/images/sensortowerR_sticker.png" width="200"></p>

`sensortowerR` is an R client for Sensor Tower's API with a tidyverse-first workflow:
search apps and publishers, resolve IDs, fetch sales and active-user metrics, and chain everything with `dplyr`/`tidyr`.

## Installation

```r
remotes::install_github("econosopher/sensortowerR")
```

## Authentication

Store your Sensor Tower API token as an environment variable:

```r
usethis::edit_r_environ()
# SENSORTOWER_AUTH_TOKEN="YOUR_SECRET_TOKEN_HERE"
```

Restart your R session after updating `.Renviron`.

## Tidyverse Quick Start

```r
library(sensortowerR)
library(dplyr)
library(tidyr)

# 1) Find the app once, keep a canonical ID
app <- st_app_info("Royal Match") %>%
  slice(1)

app_ids <- st_app_lookup(app$unified_app_id)

# 2) Pull active users (long format, ready for tidy pipelines)
active <- st_active_users(
  os = "unified",
  app_list = app$unified_app_id,
  metrics = c("dau", "mau"),
  date_range = list(start_date = "2025-01-01", end_date = "2025-12-31"),
  countries = "US",
  granularity = "monthly"
)

# 3) Pull sales for the same window
sales <- st_batch_metrics(
  os = "unified",
  app_list = app$unified_app_id,
  metrics = c("revenue", "downloads"),
  date_range = list(start_date = "2025-01-01", end_date = "2025-12-31"),
  countries = "US",
  granularity = "monthly"
)

# 4) Join and compute KPIs
active_wide <- active %>%
  group_by(original_id, app_name, date, country, metric) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = metric, values_from = value)

sales_wide <- sales %>%
  group_by(original_id, app_name, date, country, metric) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = metric, values_from = value)

kpis <- active_wide %>%
  left_join(sales_wide, by = c("original_id", "app_name", "date", "country")) %>%
  mutate(
    rev_per_dau = if_else(!is.na(dau) & dau > 0, revenue / dau, NA_real_),
    rev_per_mau = if_else(!is.na(mau) & mau > 0, revenue / mau, NA_real_)
  )
```

## Endpoint and ID Management

The package is designed so users can think in terms of analysis, not raw endpoints.

| If you need... | Use... | Behavior |
|---|---|---|
| DAU/WAU/MAU for one or many apps | `st_active_users()` or `st_batch_metrics()` | Handles active-user retrieval and returns tidy long metrics for pipelines |
| Revenue/downloads for platform-specific reporting | `st_sales_report()` | Use `os = "ios"` or `os = "android"` |
| Revenue/downloads where IDs may vary (unified, iOS, Android) | `st_metrics()` | Resolves IDs and fetches by the requested `os` |
| Cross-app workflows with mixed metrics | `st_batch_metrics()` | Batches active users + sales and standardizes shape |
| Market rankings | `st_top_charts()`, `st_top_publishers()` | Top-and-trending style analysis by region/category |

Important behavior:

- `st_sales_report()` is platform-specific and does not accept `os = "unified"`.
- `st_metrics()` is the easiest way to request unified-style sales/download workflows.
- Authentication and endpoint path construction are centralized internally, so API-facing functions now behave more consistently.

## App and Publisher Workflows

### App-first (recommended for specific apps)

```r
app <- st_app_info("Candy Crush Saga") %>%
  slice(1)

ids <- st_app_lookup(app$unified_app_id)

ios_sales <- st_sales_report(
  os = "ios",
  ios_app_id = ids$ios_app_id,
  countries = "US",
  start_date = "2025-01-01",
  end_date = "2025-01-31",
  date_granularity = "daily"
)
```

### Publisher portfolio in one call

```r
portfolio <- st_publisher_portfolio(
  publisher = "Supercell",
  metrics = c("revenue", "downloads", "mau"),
  start_date = "2023-01-01",
  countries = "WW"
)
```

### Top charts + dashboard

```r
top_games <- st_top_charts(
  measure = "revenue",
  os = "unified",
  category = 6014,
  regions = "US",
  time_range = "month"
)

st_gt_dashboard(top_games, title = "Top Games - US")
```

## Core Functions

| Function | Purpose |
|---|---|
| `st_app_info()` | Search unified apps or publishers by term |
| `st_app_lookup()` | Resolve iOS/Android/unified IDs from any input ID |
| `st_metrics()` | Flexible single-app metric retrieval with ID resolution |
| `st_sales_report()` | Platform-specific sales/download estimates |
| `st_unified_sales_report()` | Unified aggregation across related regional SKUs |
| `st_active_users()` | Tidy DAU/WAU/MAU wrapper |
| `st_batch_metrics()` | Batch retrieval across apps and metrics |
| `st_publisher_portfolio()` | End-to-end publisher analysis |
| `st_top_charts()` | Top app rankings by metric |
| `st_top_publishers()` | Publisher rankings |
| `st_category_rankings()` | Official app store ranking pulls |
| `st_gt_dashboard()` | Formatted dashboard-style output |

## Data Notes

| Data Type | Primary Function | Time Series | Coverage |
|---|---|---|---|
| Revenue/downloads (platform) | `st_sales_report()` | Yes | Country-level |
| Revenue/downloads (unified aggregation) | `st_unified_sales_report()` | Yes | Country-level |
| Revenue/downloads (ID-flexible) | `st_metrics()` | Yes | Country-level |
| DAU/WAU/MAU | `st_active_users()`, `st_batch_metrics()` | Yes | Country-level |
| Retention and demographics snapshots | `st_app_enriched()` | Snapshot | Primarily US/WW depending on metric |

## ID Cache Utilities

```r
st_cache_info()
save_id_cache()
load_id_cache()
st_clear_id_cache()
```

Cache location is CRAN-compliant via `tools::R_user_dir("sensortowerR", "cache")`.

## Learn More

- Vignette: `vignette("tidy-active-users", package = "sensortowerR")`
- Changelog: [NEWS.md](https://github.com/econosopher/sensortowerR/blob/main/NEWS.md)

## License

MIT (`LICENSE` file).
