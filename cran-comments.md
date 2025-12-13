## New submission

This is the first submission of `sensortowerR` to CRAN.

## Test environments

* local macOS (ARM64), R 4.2.2
* win-builder R-devel (Windows Server 2022 x64)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Phillip Black <pblack@gameeconomistconsulting.com>'

  New submission

The package size NOTE about installed size (~9Mb) is due to essential documentation images showing dashboard outputs and visualizations.

## Notes

* All tests that require API authentication are properly skipped when SENSORTOWER_AUTH_TOKEN is not set (as on CRAN)
* Examples are wrapped in \dontrun{} as they require API authentication
* The package interfaces with the Sensor Tower API which requires user authentication

Thanks!
