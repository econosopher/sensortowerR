# sensortowerR 2.0

Sensor Tower data through ordinary tibble pipelines. Search for apps, retain your
analysis context, and fetch sales and audiences with one function.

```r
library(sensortowerR)
library(dplyr)

st_apps("Clash of Clans", os = "unified") |>
  filter(app_name == "Clash of Clans") |>
  st_app() |>
  st_metrics(
    date_from = "2026-01-01",
    date_to = "2026-03-31",
    countries = c("US", "JP"),
    metrics = c("revenue", "downloads", "mau"),
    granularity = "monthly"
  )
```

Set `SENSORTOWER_AUTH_TOKEN` in your R environment, or supply `auth_token`.
The package never prints tokens. Both `|>` and `%>%` work. Vector inputs require
an explicit platform; tibbles supply their own `app_id` and `os` columns.

```r
st_metrics(c("529479190", "553834731"), os = "ios",
           date_from = "2026-01-01", date_to = "2026-01-31")
```

## Output and measurement

Long output contains `app_id`, `os`, `country`, `date`, `metric`, `value`, `unit`
and `period`. User columns survive row expansion. Colliding input columns get
an `.input` suffix. Results are ungrouped tibbles, including empty results.

Revenue defaults to USD dollars; downloads are counts. `shape = "wide"` provides
one value column per metric with explicit unit and period columns. Missing values
remain `NA`; they are never filled with zero.

`granularity` controls sales. DAU, WAU and MAU retain their native day, week and
month windows. Dates label period starts. Periods starting outside the requested
range are excluded; partial periods are not prorated. Unified and iOS audience
sums are labeled `platform_users` and `device_users`, respectively. They are not
deduplicated people. Audiences are never summed over time.

Use `countries = "WW"` separately from country-specific requests. Unified sales
come directly from the unified endpoint, including regional SKUs. The package
does not infer identities from names or merge related publishers.

## Main functions

| Task | Functions |
|---|---|
| Find apps and publishers | `st_apps()`, `st_publishers()` |
| Metadata and ID mapping | `st_app()`, `st_publisher_apps()` |
| Time series and market totals | `st_metrics()`, `st_market_metrics()` |
| Ranked estimates and store positions | `st_rankings()`, `st_charts()` |
| Specialist data | `st_retention()`, `st_demographics()`, `st_sessions()`, `st_ratings()`, `st_reviews()`, `st_app_tags()` |
| Filters and reference data | `st_filter()`, `st_filter_create()`, `st_filter_read()`, `st_fields()`, `st_categories()` |
| Advanced access and utilities | `st_facets()`, `st_parse_url()`, `st_build_url()`, `st_diagnostics()`, `st_cache_info()`, `st_cache_clear()` |

Metadata conversion is explicit: `st_app(data, target_os = "ios")` expands all
mapped iOS SKUs; `target_os = "unified"` uses provider mappings. Conversions retain
`input_app_id` and `input_os`. Demographics, legacy retention, ratings, reviews and
tags require store IDs; convert first when starting with unified IDs.

## Filters and failures

`st_filter()` constructs a local predicate without network activity.
`st_filter_create()` explicitly creates it on the server. A failed creation
raises an error and never manufactures an ID. AND sends all fields in one
request; OR creates one filter per field and unions matching IDs. Multiple
server writes are therefore possible for an OR filter.

```r
# This call creates server-side filters:
f <- st_filter(genre = "RPG", publisher = "Supercell", combine = "or") |>
  st_filter_create()
st_apps(filter = f)
```

Request and schema failures stop the pipeline by default. `errors = "partial"`
on data-first retrieval functions returns identifiable failure rows with `NA`
values, `status`, `error` and `endpoint`, plus a warning. A successful empty result
is distinct from an error. Sales and audience requests do not silently substitute
one platform's data for a failed combined result.

Caching is off by default. `cache = TRUE` on `st_metrics()` enables a session-only,
credential-scoped cache for 300 seconds (`cache_ttl` changes this). Partial results
and errors are not cached. Use `st_cache_clear()` to clear entries.

## Reports and migration

Portfolio totals, YoY comparisons, charts, formatting and dashboards now use
recipes built from dplyr, ggplot2, scales and gt. See `vignette("recipes")` and
`inst/recipes/reports.R`. See [the migration table](https://github.com/econosopher/sensortowerR/blob/02daa2174cf36a002bfab265c98f37d6cdde3040/MIGRATION.md) for every v1.x export.
This is a breaking 2.0.0 candidate; existing scripts need migration.

## Verification

Run `Rscript tools/check.R` for offline tests, build and package checks. Run
`SENSORTOWER_RUN_LIVE=true Rscript tools/live-audit.R` separately for bounded
read-only checks. Live results, unavailable entitlements and empty responses are
recorded separately in [the audit directory](https://github.com/econosopher/sensortowerR/tree/codex/sensortower-v2/audit). Filter creation is tested
offline. A successful package check is not a claim that every API entitlement is
available.
