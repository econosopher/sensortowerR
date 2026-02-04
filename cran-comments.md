## Resubmission

This is a resubmission. In this version I have:

* **FIXED** `st_game_summary(os = "unified")` returning empty results
  - The `games_breakdown` API only supports `ios` and `android`
  - Unified mode now fetches both platforms and combines results
  - Adds `Total Downloads` and `Total Revenue` columns in unified mode
  - Documentation updated to clarify unified behavior

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
