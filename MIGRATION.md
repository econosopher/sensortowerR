# Migrating every v1.x export to 2.0

This is a clean break. No deprecated wrappers or defunct aliases remain.
The inventory covers all 73 exports in installed v1.0.1. Input tables carry
`app_id` and `os`; vectors require `os`. Dates are explicit, long output is the
default, revenue is USD, and missing values remain missing. User context survives
pipeline steps. Grouping is dropped. API errors stop the pipeline unless partial
mode is explicitly selected.

`country`/`regions` become `countries`; `start_date`/`end_date` become
`date_from`/`date_to`; `date_granularity` becomes `granularity`. `st_charts()` keeps
`date` for a single chart snapshot. `st_metrics()` supports native audience windows
independently of sales granularity. `st_facets()` preserves access to specialist
breakdowns and provider options through named parameters.

| v1.x export | v2 replacement | Migration note |
|---|---|---|
| `calculate_yoy_growth` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `example_sensortower_data` | `Explicit synthetic tibbles in recipes` |  |
| `format_arpu` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `format_currency` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `format_downloads` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `format_large_number` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `format_market_share` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `format_percent` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `format_retention` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `format_users` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `lookup_category_names` | `st_categories() with dplyr::left_join()` |  |
| `st_active_users` | `st_metrics()` | Native period is retained; no summing DAU into MAU. Unified sums are platform_users. |
| `st_analyze_filter` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `st_api_diagnostics` | `st_diagnostics() and explicit retrieval` |  |
| `st_app` | `st_app()` |  |
| `st_app_details` | `st_app()` |  |
| `st_app_enriched` | `st_app()` |  |
| `st_app_info` | `st_app()` | Use st_apps() or st_publishers() for search, st_app() for ID lookup. |
| `st_app_lookup` | `st_app()` |  |
| `st_app_tag` | `st_apps()` |  |
| `st_apps` | `st_apps()` |  |
| `st_batch_app_lookup` | `st_app()` |  |
| `st_batch_metrics` | `st_metrics()` |  |
| `st_build_filter_url` | `st_build_url()` |  |
| `st_build_web_url` | `st_build_url()` |  |
| `st_cache_info` | `st_cache_info()` |  |
| `st_categories` | `st_categories() with dplyr::left_join()` |  |
| `st_category_rankings` | `st_charts()` |  |
| `st_clear_app_cache` | `st_cache_clear()` |  |
| `st_clear_id_cache` | `st_cache_clear()` |  |
| `st_combine_filters` | `st_filter() then st_filter_create()` |  |
| `st_compare_filter_results` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `st_create_simple_filter` | `st_filter() then st_filter_create()` |  |
| `st_custom_fields_filter` | `st_filter() then st_filter_create()` | Creation is explicit and returns only verified server IDs. |
| `st_custom_fields_filter_by_id` | `st_filter(filter_id = ...)` | Use st_filter_read() for server definitions; pass existing IDs directly to consumers. |
| `st_custom_fields_values` | `st_fields()` |  |
| `st_demographics` | `st_demographics()` |  |
| `st_discover_fields` | `st_fields()` |  |
| `st_extract_filter_id` | `st_filter(filter_id = ...)` | Use st_parse_url() to read the ID before constructing the filter. |
| `st_extract_url_params` | `st_parse_url()` |  |
| `st_facets_metrics` | `st_facets()` |  |
| `st_filter` | `st_filter() then st_filter_create()` |  |
| `st_filter_by_date` | `st_filter() then st_filter_create()` | Release-date criteria remain available through st_filter(date_from, date_to). |
| `st_filter_by_genre` | `st_filter() then st_filter_create()` |  |
| `st_filter_by_monetization` | `st_filter() then st_filter_create()` |  |
| `st_filter_by_publisher` | `st_filter() then st_filter_create()` |  |
| `st_filter_by_sdk` | `st_filter() then st_filter_create()` |  |
| `st_game_summary` | `st_market_metrics()` |  |
| `st_generate_example_filter_ids` | `Local st_filter() objects; no generated placeholder IDs` |  |
| `st_get_app_names` | `st_app()` | Use st_app(fields = "app_name") to join metadata. |
| `st_get_filter_collection` | `Local st_filter() objects; no generated placeholder IDs` |  |
| `st_get_filtered_apps` | `st_apps()` |  |
| `st_get_unified_mapping` | `st_app(target_os = ...)` | All regional mappings remain visible; no name-based matching. |
| `st_gt_dashboard` | `Report recipes (dplyr, ggplot2, scales, gt)` |  |
| `st_is_valid_filter_id` | `st_filter(filter_id = ...)` |  |
| `st_market_metrics` | `st_market_metrics()` |  |
| `st_metrics` | `st_metrics()` |  |
| `st_parse_web_url` | `st_parse_url()` |  |
| `st_publisher_apps` | `st_publisher_apps()` |  |
| `st_publisher_portfolio` | `Report recipes (dplyr, ggplot2, scales, gt)` | Compose publisher_apps and metrics; recipes aggregate sales only. |
| `st_rankings` | `st_rankings(measure = ..., entity = ...)` | Specify measure explicitly. Former category chart requests use st_charts(). |
| `st_ratings_facets` | `st_ratings()` |  |
| `st_retention` | `st_retention(method = ...)` | Facets is the default. Legacy returned HTTP 404 in this audit and is marked unavailable. |
| `st_retention_facets` | `st_retention(method = ...)` |  |
| `st_reviews_by_rating_facets` | `st_reviews()` |  |
| `st_sales_report` | `st_metrics()` |  |
| `st_session_metrics` | `st_sessions()` |  |
| `st_smart_metrics` | `st_metrics()` |  |
| `st_test_filter` | `st_diagnostics() and explicit retrieval` |  |
| `st_top_charts` | `st_rankings(measure = ..., entity = ...)` |  |
| `st_top_publishers` | `st_rankings(measure = ..., entity = ...)` |  |
| `st_unified_sales_report` | `st_metrics()` |  |
| `st_yoy_metrics` | `Report recipes (dplyr, ggplot2, scales, gt)` | Use explicit date ranges and recipe_yoy() on corresponding periods. |

Reports are executable recipes in `inst/recipes/reports.R`. The cache has an
explicit TTL and no disk storage. Existing persisted caches are not migrated.
