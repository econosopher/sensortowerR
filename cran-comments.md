Update of the existing CRAN package sensortowerR from 2.0.0 to 2.1.0.
The maintainer and MIT license are unchanged.

This minor release adds two exports, st_ad_monetization() and
st_ad_rankings(), for the provider's ad monetization estimates; adds an
optional `tags` argument to st_rankings() that returns requested custom tags
as columns; and fixes st_filter_create() for named custom fields (the API
rejected the request body). Other existing functions are unchanged. NEWS.md
documents the changes.

The final source archive was built with R 4.6.2 on Ubuntu 24.04 arm64.
R CMD check --as-cran on that environment (PDF manual included): 0 errors, 0 warnings, 0 notes.
Windows R-release 4.6.2: 0 errors, 0 warnings, 0 notes.
Windows R-devel (2026-11-14 r90890): 0 errors, 0 warnings, 0 notes.
macOS, R 4.2.2, local (no manual): 0 errors, 0 warnings, 0 notes.
Mac Builder was unavailable on 2026-11-16 and could not be run.
All 272 test assertions pass without credentials or API network access.
The CRAN index was checked on 2026-11-16: there are no reverse dependencies.
URL checks pass.

Live retrieval requires a user-provided Sensor Tower subscription/API token.
Ordinary package checks do not call the service. The optional authenticated
live audit is excluded from the package archive. HTTP and schema failures
produce informative errors. Caching is opt-in and session-only.
