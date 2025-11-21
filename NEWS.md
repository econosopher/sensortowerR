# sensortowerR 0.8.3

## Internal Refactoring & Robustness

* **Refactored Core Logic**: Streamlined `st_batch_metrics` and `st_metrics` to use centralized data fetching and constant definitions.
* **Improved Stability**: Fixed regression in `resolve_ids_for_os` to ensure consistent ID handling across platforms.
* **Enhanced Testing**: Expanded test suite to cover edge cases in batch processing and ID resolution.

# sensortowerR 0.8.2

## Critical Fix

* **FIXED** DAU/WAU/MAU measures now correctly sort when using custom filters
  - Previously routed incorrectly to `active_users` endpoint
  - Now all measures use `sales_report_estimates_comparison_attributes` endpoint
  - Custom filters from web URLs now work correctly with DAU sorting
  - Use `measure = "revenue"` when working with custom filters that sort by DAU

## Documentation Updates

* Updated `st_top_charts()` documentation to reflect endpoint changes
* Added examples showing how to use custom filter URLs from web interface
* Clarified that all measures now use the sales endpoint

# sensortowerR 0.8.0

## Breaking Changes

* **REMOVED** `app_ids` parameter from `st_sales_report()`. This is a breaking change that requires updating existing code.
  - Use the new specific parameters instead: `ios_app_id`, `android_app_id`, or `unified_app_id`
  - For batch processing multiple apps, use `purrr::map()` or a loop instead of passing a vector
  - The new parameters provide better type safety and consistency with `st_metrics()`

## Improvements

* `st_sales_report()` now uses the same ID resolution system as `st_metrics()`
  - Automatic cross-platform ID lookup when providing mismatched IDs
  - Clear error messages for invalid ID formats
  - Support for unified IDs with automatic platform resolution

# sensortowerR 0.7.4

## New features

* Added URL parsing utilities:
  - `st_parse_web_url()` - Convert Sensor Tower web URLs to API parameters
  - `st_extract_url_params()` - Extract and analyze all parameters from web URLs
  - `st_build_web_url()` - Build web URLs from API parameters (reverse operation)
  - Handles parameter mapping between web interface and API conventions
  - Automatically converts extensive country lists to "WW" when appropriate

# sensortowerR 0.7.3

## New features

* Enhanced custom filter support in `st_top_charts()`:
  - `category` parameter is now optional when `custom_fields_filter_id` is provided
  - Supports filters created in the Sensor Tower web interface
  - Added comprehensive example script demonstrating custom filter usage
  - Updated documentation explaining how to obtain filter IDs

# sensortowerR 0.7.2

## New features

* Added active user metrics (DAU, WAU, MAU) support to `st_batch_metrics()`
  - Active user metrics are fetched using efficient batch API calls
  - Supports all platforms: iOS, Android, and unified
  - Automatically maps granularity to appropriate time periods for each metric
  - Includes rate limit warnings for large batches (>10 apps)
  - Seamlessly integrates with existing revenue and download metrics

## Testing improvements

* Consolidated test scripts from 6 files to 2 comprehensive test suites
* Enhanced test coverage for various app ID types, granularities, and countries
* Added comprehensive validation of active user metric relationships (DAU < WAU < MAU)
* Improved test efficiency to minimize API calls while maintaining coverage

# sensortowerR 0.7.1

## Bug fixes

* Fixed `st_batch_metrics()` ID resolution issue where it was incorrectly using OS value instead of app ID
* Fixed column type mismatch when combining active user results with revenue/download results
* All 9 major functions now pass comprehensive API tests with 100% success rate

# sensortowerR 0.7.0

## BREAKING CHANGES

* Removed `st_ytd_metrics()` function - use `st_metrics()` or `st_batch_metrics()` with specific date ranges instead
* Removed `st_publisher_category_breakdown()` function - category-level publisher analysis no longer supported  
* Removed automatic ID resolution - users must now specify exactly one type of ID at a time:
  - Publisher ID
  - Unified app ID (24-character hex)
  - iOS app ID (numeric)
  - Android app ID (package name)

## Major improvements

* Reimplemented `st_yoy_metrics()` to work without dependency on `st_ytd_metrics()`
  - Now uses `st_batch_metrics()` internally for more efficient data fetching
  - Maintains all previous functionality including automatic YoY calculations
  - Better error handling and date validation

## Other changes

* Fixed all references to removed functions throughout the package
* Updated documentation and tests to reflect changes
* Fixed dplyr compatibility issues with .data pronoun
* Package now passes R CMD check --as-cran with only acceptable NOTEs

# sensortowerR 0.1.7

## Major improvements

* `st_metrics()` has been completely rewritten to automatically handle daily data limitations:
  - Now automatically uses platform-specific endpoints when daily data is requested
  - Intelligently detects whether app_id is iOS (numeric) or Android (package name)
  - Combines iOS and Android data by default for true unified metrics
  - Maintains backward compatibility with deprecated `unified_app_id` parameter

## New features in st_metrics()

* New parameter `app_id` replaces `unified_app_id` (old parameter still works with deprecation warning)
* New parameters `ios_app_id` and `android_app_id` for explicit platform specification
* New parameter `combine_platforms` (default TRUE) to control platform data combination
* New parameter `date_granularity` supporting "daily", "weekly", "monthly", "quarterly"
* New parameter `countries` for specifying country codes
* New parameter `auto_platform_fetch` (default TRUE) to control automatic endpoint switching
* New parameter `verbose` to control progress messages

## Bug fixes

* Fixed issue where `st_metrics()` returned 0 rows for daily data requests
* Fixed data type mismatches when combining iOS and Android data

## Documentation

* Updated README with new st_metrics() usage examples
* Added MIGRATION_GUIDE.md for upgrading existing code
* Added ENDPOINT_REFERENCE.md documenting which endpoints work for daily data
* Added comprehensive test scripts in tests/ directory

# sensortowerR 0.1.6

* Previous version changes...