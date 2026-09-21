# CRAN readiness: 2.0.0 update

The user authorized submission as an update to existing CRAN package
sensortowerR 1.0.1. The maintainer and MIT license are unchanged. GitHub Actions
is disabled. All three release-platform checks and Windows R-devel passed. The candidate
was merged in PR #7; CRAN submission is the next step.

## Final source archive

Built with R 4.6.1 on a temporary local Ubuntu 24.04.4 arm64 VM from package
source commit 3ae6f1c997f2f2c4fc6f9aa849e616c32bc4eaa7. The exact archive is
sensortowerR_2.0.0.tar.gz, SHA-256:

`62b23c5ac94847776a2c1652fac86634d85f1c9983fe36f5328b2dac576f9254`

The archive contains 60 entries and is 42,766 bytes. Its package source files
match the checkout. It contains no raw live API receipts or credential patterns.

## Verified checks

| Environment or check | Result | Evidence |
|---|---|---|
| Ubuntu 24.04.4 arm64, R 4.6.1, R CMD check --as-cran | 0 errors, 0 warnings, 0 notes | cran-linux-check.log |
| Mac Builder arm64, R 4.6.1 Patched | 0 errors, 0 warnings, 0 notes | cran-mac-builder-check.log |
| Windows R-release 4.6.1 | 0 errors, 0 warnings, 0 notes | cran-windows-release-check.log |
| Windows R-devel 2026-09-20 r90574 | 0 errors, 0 warnings, 0 notes | cran-windows-devel-check.log |
| Offline assertions | 191 passed, 0 failed, 0 warned, 0 skipped | Linux test log and offline-tests.csv |
| README and documentation URLs | All five external URLs passed | urlchecker::url_check() |
| CRAN reverse dependencies | None, including optional dependencies | cran-reverse-dependencies.json |
| Live API audit | 24 verified cases; legacy retention HTTP 404 | live-audit.csv |

Linux checked the examples, rebuilt vignettes, PDF manual and HTML manual.
Mac Builder also checked the PDF manual; its arguments were --no-clean-on-error.
The local Linux VM is stopped. Dependency versions are recorded in
cran-linux-dependencies.csv.

Initial Windows checks passed the code but noted a possible spelling issue in
DESCRIPTION and two README file links excluded from the archive. These are fixed
in the final source. Historical Windows and R 4.2.2 logs refer to earlier
archives and do not replace the final Windows checks. The current status and
builder URLs are recorded in cran-builder-status.json.

## Before submission

Submit this exact tested archive through CRAN's submission
form and complete the maintainer email confirmation. No duplicate submission
should be made while one is pending. The installed v1.0.1 package and original
checkout edits remain unchanged.

## Primary guidance

- [CRAN policies](https://cran.r-project.org/web/packages/policies.html)
- [Submission checklist](https://stat.ethz.ch/CRAN/web/packages/submission_checklist.html)
- [Windows builder](https://win-builder.r-project.org/)
- [Mac Builder](https://mac.r-project.org/macbuilder/submit.html)
