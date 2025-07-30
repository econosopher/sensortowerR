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