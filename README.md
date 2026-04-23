# sensortowerR

<p align="center"><img src="inst/images/SensorTowerR_sticker.png" width="200"></p>

`sensortowerR` is a tidyverse-first R client for Sensor Tower's mobile app intelligence API. Four core verbs — **`st_metrics()`**, **`st_rankings()`**, **`st_app()` / `st_apps()`**, **`st_filter()`** — cover the vast majority of workflows with consistent parameters and long-format tibble output.

> **Upgrading from 0.9.x?** See [`vignette("migrating-to-1.0")`](vignettes/migrating-to-1.0.Rmd). Revenue is now in dollars by default (was cents). Most 0.9.x functions are `.Defunct()` stubs that will tell you exactly what to replace them with.

## Installation

```r
# From GitHub
remotes::install_github("econosopher/sensortowerR")

# From CRAN (once 1.0.0 is released)
install.packages("sensortowerR")
```

## Authentication

Store your Sensor Tower API token as an environment variable:

```r
usethis::edit_r_environ()
# SENSORTOWER_AUTH_TOKEN="YOUR_SECRET_TOKEN_HERE"
```

Restart your R session after updating `.Renviron`.

## The four core verbs

### `st_metrics()` — revenue and downloads

```r
library(sensortowerR)
library(dplyr)

# Single app, unified across iOS + Android
royal_match <- st_apps("Royal Match") |> slice(1) |> pull(app_id)

sales <- st_metrics(
  app_id      = royal_match,
  metrics     = c("revenue", "downloads"),
  countries   = c("US", "JP", "GB"),
  date_from   = "2025-01-01",
  date_to     = "2025-12-31",
  granularity = "monthly"
)

# Returns long format by default: app_id, os, country, date, metric, value
# Revenue is in dollars. Pass `revenue_unit = "cents"` for raw integer cents.
```

### `st_rankings()` — top charts, publishers, categories

```r
top_ios_games <- st_rankings(
  entity   = "app",
  os       = "ios",
  category = 6014,
  country  = "US",
  limit    = 50
)

top_publishers <- st_rankings(
  entity  = "publisher",
  os      = "unified",
  country = "US"
)
```

### `st_app()` and `st_apps()` — lookup vs. search

```r
# Direct ID lookup
st_app(app_id = "55c5022f02ac64f9c0001f9f")

# Discovery by name
st_apps(query = "candy crush", os = "ios", limit = 10)

# Discovery by filter
rpg_filter <- st_filter(genre = "rpg", monetization = "in_app_purchases")
rpg_apps   <- st_apps(filter = rpg_filter, limit = 50)
```

### `st_filter()` — the builder

`st_filter()` returns an `st_filter` S3 object that `st_apps()`, `st_rankings()`, and `st_get_filtered_apps()` all accept. Compose filters with `c()`.

```r
us_rpgs_2025 <- st_filter(
  date_from    = "2025-01-01",
  date_to      = "2025-12-31",
  genre        = "rpg",
  monetization = "free"
)

print(us_rpgs_2025)  # Human-readable summary
as.character(us_rpgs_2025)  # Server-side filter ID

# Combine two filters
combined <- c(us_rpgs_2025, st_filter(sdk = "unity"))
```

## Function index

### Core (4 unified verbs)

| Function | Purpose |
|---|---|
| `st_metrics()` | Revenue / downloads across one or many apps, countries, dates |
| `st_rankings()` | Top charts / publishers / categories |
| `st_app()` / `st_apps()` | ID lookup / name search / filter-based discovery |
| `st_filter()` | Build a reusable filter object |

### Analytics

| Function | Purpose |
|---|---|
| `st_active_users()` | DAU / WAU / MAU time series |
| `st_retention()` | D1–D90 retention curves |
| `st_retention_facets()` | Demographic retention breakdowns |
| `st_ratings_facets()` | Ratings by demographic facets |
| `st_reviews_by_rating_facets()` | Review counts per star rating |
| `st_session_metrics()` | Session counts, duration, time spent |
| `st_demographics()` | Age / gender breakdowns |
| `st_app_enriched()` | Multi-metric enrichment for known apps |
| `st_yoy_metrics()` | Year-over-year comparison helper |
| `st_game_summary()` | Game-specific genre / subgenre summary |

### Publishers

| Function | Purpose |
|---|---|
| `st_publisher_apps()` | All apps owned by a publisher |
| `st_publisher_portfolio()` | End-to-end publisher portfolio analysis |

### Low-level / power-user

| Function | Purpose |
|---|---|
| `st_facets_metrics()` | Direct `/facets/metrics` access |
| `st_get_filtered_apps()` | Raw filter-ID app listing |
| `st_test_filter()`, `st_analyze_filter()` | Validate and inspect filters |
| `st_discover_fields()`, `st_custom_fields_values()` | Introspect available filter fields |
| `st_api_diagnostics()` | Debug API request issues |
| `st_get_unified_mapping()`, `st_batch_app_lookup()` | Platform ID ↔ unified-ID resolution |
| `st_categories()` | Full category list |

### Utilities

| Function | Purpose |
|---|---|
| `st_gt_dashboard()` | Formatted `gt` table output |
| `st_cache_info()`, `st_clear_id_cache()`, `st_clear_app_cache()` | Cache management |
| `st_build_web_url()`, `st_parse_web_url()` | App-ID ↔ web URL |
| `format_currency()`, `format_downloads()`, `format_percent()`, `format_retention()`, `format_market_share()`, `format_arpu()`, `format_users()`, `format_large_number()` | Human-readable number formatters |
| `calculate_yoy_growth()`, `lookup_category_names()` | Analysis helpers |
| `example_sensortower_data()` | Sample data for examples (no API call) |

## Data notes

| Data Type | Function | Time Series | Coverage |
|---|---|---|---|
| Revenue / downloads | `st_metrics()` | Yes | Country-level |
| DAU / WAU / MAU | `st_active_users()` | Yes | Country-level |
| Retention | `st_retention()`, `st_retention_facets()` | Snapshot | US / WW primarily |
| Ratings / reviews | `st_ratings_facets()`, `st_reviews_by_rating_facets()` | Snapshot | WW |
| Rankings | `st_rankings()` | Point-in-time | Country-level |

## Parameter conventions (v1.0.0)

Every function uses the same parameter names:

| Parameter | Meaning |
|---|---|
| `app_id` | Scalar or vector of app IDs (unified, iOS, or Android — auto-resolved) |
| `os` | `"ios"`, `"android"`, or `"unified"` |
| `country` / `countries` | ISO-2 code (scalar) or vector of codes |
| `date_from`, `date_to` | Dates or ISO strings |
| `granularity` | `"daily"`, `"weekly"`, `"monthly"`, `"quarterly"` |
| `metrics` | Character vector of metric names |
| `auth_token` | Optional override of the env-var token |
| `limit` | Row cap |

## Learn more

- Vignette: `vignette("migrating-to-1.0", package = "sensortowerR")`
- Vignette: `vignette("tidy-active-users", package = "sensortowerR")`
- Vignette: `vignette("custom-fields", package = "sensortowerR")`
- Changelog: [NEWS.md](https://github.com/econosopher/sensortowerR/blob/main/NEWS.md)

## License

MIT (`LICENSE` file).
