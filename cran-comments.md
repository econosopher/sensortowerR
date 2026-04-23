## Resubmission — major version bump

This is an update to the existing CRAN package `sensortowerR` (currently `0.9.4` on CRAN). Version `1.0.0` is a major API simplification: the public surface is reduced from 78 exports to roughly 50 through four new unified functions (`st_metrics()`, `st_rankings()`, `st_app()` / `st_apps()`, `st_filter()`). Redundant and inconsistently-named predecessors are retained as `.Defunct()` stubs that error with a clear migration message pointing at the replacement, so users see exactly what to change.

### Breaking changes documented in NEWS.md

* Revenue is returned in dollars by default (was raw cents). Opt back in via `revenue_unit = "cents"`.
* 21 functions are now defunct — each one errors with a one-line migration hint.
* Parameters are standardized across all retained functions (for example, `ios_app_id`/`unified_app_id` → `app_id`; `date_granularity` → `granularity`; `measure` → `metrics`; `regions` → `countries`).
* Eleven internal helpers that had been accidentally exported in earlier releases are now properly internal.

### Reverse dependency impact

No reverse dependencies on CRAN (verified before submission via `tools::package_dependencies("sensortowerR", reverse = TRUE, db = available.packages())`). Users of the 0.9.x API will see defunct errors with replacement hints on their next run — no silent misbehavior.

## Test environments

* local macOS (ARM64), R 4.2.2
  - `testthat::test_local()` — all tests pass
  - live authenticated smoke tests against the Sensor Tower API for `st_metrics`, `st_rankings`, `st_app`, `st_apps`, `st_filter`, `st_active_users`, `st_retention`
  - `rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))`

## R CMD check results

0 errors | 0 warnings | 1 note:
* `unable to verify current time` — environmental, unrelated to package

## Notes

* All examples that require API authentication remain wrapped in `\dontrun{}`
* The package requires a user-supplied Sensor Tower API token (env var `SENSORTOWER_AUTH_TOKEN`) and does not hardcode credentials
* No writes to the user's home filespace; cache is process-local by default
* New `st_filter` S3 class has `print`, `format`, `c`, and `as.character` methods
