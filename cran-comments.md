## Resubmission

This is a resubmission (version 0.9.6). In this version I have:

* Added `st_active_users()` as a focused, tidy long-format wrapper for DAU/WAU/MAU workflows
* Added a new vignette: **Tidy Active User Workflows**
* Centralized endpoint-path construction through a shared endpoint registry helper
* Centralized API token resolution/validation through a shared helper
* Refactored active-user batching logic to reduce request fan-out and improve readability
* Removed test warning noise by updating deprecated tidyselect usage and test expectation syntax

## Test environments

* local macOS (ARM64), R 4.2.2 (`devtools::check(cran = TRUE, manual = FALSE)`)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

This NOTE appears to be environment-specific in this check runtime.

## Notes

* All tests that require API authentication are properly skipped when SENSORTOWER_AUTH_TOKEN is not set (as on CRAN)
* Examples are wrapped in \dontrun{} as they require API authentication
* The package interfaces with the Sensor Tower API which requires user authentication
* No functions write to user's home filespace by default
