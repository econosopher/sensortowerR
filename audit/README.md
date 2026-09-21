# 2.0.0 candidate audit

## Baseline and boundaries

The implementation starts at upstream commit
`abe3624bbdac5e9cf33efed8c95b3d3cfe290388` (v1.0.0) and compares the installed
v1.0.1 namespace. `installed-comparison.json` records the five changed function
bodies and additional installed helpers. The 73-export inventory and complete
migration mapping are `v1-exports.txt` and `export-migration.csv`.

The rewrite preserves explicit canonical identity, native audience windows,
regional store mappings, strict duplicate checks and revenue conversion. It
removes name-based identity guesses, swallowed failures, fabricated filter IDs,
zero-filled missing metrics, and separate single/batch retrieval paths.

This is a release candidate, not an installed update or published release.
Existing local edits in the original v0.1.0 checkout remain untouched.

## Offline verification

The deterministic suite passes 37 tests and 191 assertions with no failures.
`offline-tests.csv` records each test; `macos-local-check.log` records a clean
R 4.2.2 macOS check with no errors, warnings or notes.

The deterministic suite covers both pipe operators, full app and publisher
pipelines, duplicate inputs and context preservation, metadata conversion,
single/vector equivalence, request chunking, offset/cursor pagination, missing
values, units, duplicate conflicts, partial results, credential redaction,
bounded retries (including Retry-After), expiring credential-scoped caching,
AND/OR filter request plans, specialist schemas, and reporting recipes.

`tools/check.R` runs the suite, builds vignettes, installs into R's temporary check
library and runs R CMD check locally. See `TESTING.md` for the local workflow.

GitHub Actions was initially attempted for Linux, Windows and R-release macOS,
but no steps executed: GitHub reported an account billing lock.
`ci-status.json` preserves that historical receipt. At the user's request,
both remote workflows (R package checks and R-hub) were disabled, and their
configuration files were removed from the candidate and pushed. No additional
GitHub compute is part of validation.

The final archive was built with R 4.6.1 on a local Ubuntu 24.04 VM. Linux
`R CMD check --as-cran` and Mac Builder R 4.6.1 Patched passed with no errors,
warnings or notes. Windows R-release 4.6.1 also passed with no errors, warnings
or notes. Windows R-devel also passed with no errors, warnings or notes. Historical R 4.2.2
receipts remain available but do not identify the final source archive.
See `CRAN-READINESS.md` for the stricter CRAN checks and remaining release gates.
The historical GitHub lock is not a dependency of building or using the package.

## Live evidence

`live-audit.csv` records each check, its timestamp, request count and outcome.
`tools/live-audit.R` is opt-in, permits only GET requests, and limits one run to
40 logical requests (each can make at most three transport attempts). The sample
covers app and publisher discovery, unified/store metadata, ID conversion,
unified/iOS/Android sales, multiple unified apps, DAU/WAU/MAU, sales and active
rankings, publisher rankings, charts, market totals, retention, demographics,
sessions, ratings, reviews, tags and custom fields.

Every `live_verified` row means the request and parser succeeded and an
independent comparison matched raw IDs or metric values. Metric arithmetic is
checked outside package normalization helpers. `live_empty` is not verification
of numeric data. `unavailable` records an actual provider response or missing
credential. `failed` means the candidate needs investigation.

The last full live pass checked 25 cases: 24 verified, one unavailable. The
legacy `/v1/ios/usage/retention` endpoint returned HTTP 404. The default facets
retention endpoint returned data and passed value comparisons. Legacy retention
continues to fail explicitly; it never becomes empty success or silently changes
method. The audit does not establish whether the legacy route is retired for
all accounts.

Filter creation is intentionally offline-tested only, because it changes server
state. Filter definition reads and cursor-based filtered discovery are also
fixture-tested, not live-verified in this sample. Bundled category data is a
reference snapshot. Live coverage is limited to the sampled apps, periods and
parameters; it does not establish every entitlement or parameter combination.

Live checks discovered and corrected multi-row audience handling, store metadata
containing both store and unified IDs, required tag-field selection, the platform
publisher path, and current demographic age-band fields. Each has a regression
test or fixture assertion.

Raw API response receipts remain local in ignored `live-responses.json`; paid
estimates are not committed. `tools/compare-live.R` contains the independent
checks. No token values, authentication headers, or credential-bearing URLs are
written to the published audit.
