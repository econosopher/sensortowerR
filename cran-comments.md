Update of the existing CRAN package sensortowerR from 2.0.0 to 2.1.0.
The maintainer and MIT license are unchanged.

This minor release adds two exports, st_ad_monetization() and
st_ad_rankings(), for the provider's ad monetization estimates, and fixes
st_filter_create() for named custom fields (the API rejected the request
body). Other existing functions are unchanged. NEWS.md documents the additions. The 2.0.0 check
found no reverse dependencies; it has not been repeated for 2.1.0.

R CMD check --as-cran on macOS, R 4.2.2, local: the code, examples, tests and
rebuilt vignettes passed. The PDF manual could not be built because the local
TeX installation is Intel-only; with --no-manual the result was 0 errors,
0 warnings and 1 note ("unable to verify current time", a local network note).
Checks on current R-release, R-devel, Windows and Mac Builder have not yet been
run for 2.1.0.
All 265 test assertions pass without credentials or API network access.

Live retrieval requires a user-provided Sensor Tower subscription/API token.
Ordinary package checks do not call the service. The optional authenticated
live audit is excluded from the package archive. HTTP and schema failures
produce informative errors. Caching is opt-in and session-only.
