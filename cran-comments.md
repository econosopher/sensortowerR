## Resubmission

This is a resubmission. In this version I have:

* **FIXED** the CRAN policy violation regarding writing to user's home filespace:
  - `st_publisher_portfolio()`: Changed `cache_dir` default from `"data/"` to `NULL`
  - Caching is now disabled by default; users must explicitly set `cache_dir` to enable
  - Removed automatic cache saving in ID resolution functions
  - Updated tests to use `tempdir()` instead of default cache paths
  - Added `tests/manual` to `.Rbuildignore`

## Test environments

* local macOS (ARM64), R 4.2.2
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Phillip Black <pblack@gameeconomistconsulting.com>'

This is a resubmission following CRAN feedback.

## Notes

* All tests that require API authentication are properly skipped when SENSORTOWER_AUTH_TOKEN is not set (as on CRAN)
* Examples are wrapped in \dontrun{} as they require API authentication
* The package interfaces with the Sensor Tower API which requires user authentication
* No functions write to user's home filespace by default

Thanks!
