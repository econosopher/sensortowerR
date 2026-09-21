## Update from 1.0.1 to 2.0.0

This is a breaking update to the existing CRAN package. It replaces 73 exports
with 25 consistent functions for data-first tibble pipelines. NEWS.md describes
the changes, and MIGRATION.md in the source repository maps every previous
export. The maintainer and license are unchanged.

## Checks completed

* Local macOS arm64, R 4.2.2: R CMD check --as-cran, including PDF manual,
  examples, tests and rebuilt vignettes: 0 errors, 0 warnings, 1 note.
  The note is "unable to verify current time". It is an environment check;
  no package code or documentation issue was reported.
* R Mac Builder, macOS arm64, R 4.6.1 Patched: 0 errors, 0 warnings, 0 notes.
  This service ran R CMD check with --no-clean-on-error, not --as-cran.
* URL checks: all three external URLs passed.
* CRAN package index checked on 2026-09-21: no reverse dependencies in
  Depends, Imports, LinkingTo, Suggests or Enhances.

## Pending before submission

Windows R-devel was uploaded to win-builder; its result is pending. Linux has
not been checked. Build the final submission archive using current R-release or
R-patched and check that exact archive before uploading to CRAN. This document
is a draft and must be updated with final receipts before submission.

## API access

Ordinary tests and runnable examples require no API token or network access.
Live retrieval requires the user's Sensor Tower subscription and API token.
The optional live audit is excluded from the source package. HTTP and response
errors produce informative conditions. Caching is opt-in and session-only;
package use does not write into the user's home directory.
