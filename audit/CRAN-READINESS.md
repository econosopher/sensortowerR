# CRAN readiness: 2.1.0 update

2.1.0 adds st_ad_monetization() and st_ad_rankings(), the st_rankings() `tags` argument, and fixes st_filter_create() for
named custom fields (PRs #8, #9 and #10; main at 6c6b949). CRAN has 2.0.0. 2.1.0 was submitted and confirmed on
2026-11-16 (see CRAN-SUBMISSION.md); publication pending.

## Final source archive (2.1.0)

Built with R 4.6.2 on a temporary Ubuntu 24.04.4 arm64 Lima VM from commit 6c6b949. sensortowerR_2.1.0.tar.gz,
48,547 bytes, 63 entries, SHA-256 `609155412b53a6f0d608524f6b71ef99727634c88ccb33b0bbd212e93bc50e55`. An earlier candidate built from 4b509f9 (SHA-256 2bb87b4b...f96475)
is superseded: PR #10 merged after it was built.

## Verified checks (2.1.0, 16 Nov 2026)

| Environment or check | Result | Evidence |
|---|---|---|
| Ubuntu 24.04.4 arm64, R 4.6.2, R CMD check --as-cran, PDF manual | 0 errors, 0 warnings, 0 notes | cran-linux-check.log |
| Windows R-release (4.6.2 (2026-10-29 ucrt)) | 0 errors, 0 warnings, 0 notes | cran-windows-release-check.log |
| Windows R-devel (R-devel 2026-11-14 r90890 ucrt) | 0 errors, 0 warnings, 0 notes | cran-windows-devel-check.log |
| macOS arm64, R 4.2.2, tools/check.R (no manual) | Status OK | macos-local-check.log |
| Mac Builder | Unavailable (6 attempts, nginx error) | cran-builder-status.json |
| Offline assertions | 272 passed, 0 failed, 0 warned, 0 skipped | macOS test run |
| README and documentation URLs | All five passed | urlchecker::url_check() |
| CRAN reverse dependencies | None, including optional dependencies | cran-reverse-dependencies.json |

The VM was deleted after the check. Windows checks used the exact archive above.

---

# Previous release: 2.0.0

The user authorized submission as an update to existing CRAN package
sensortowerR 1.0.1. The maintainer and MIT license are unchanged. GitHub Actions
is disabled. All three release-platform checks and Windows R-devel passed. The candidate
was merged in PR #7. CRAN submission is confirmed and in pretest; publication
is pending. See CRAN-SUBMISSION.md for the receipt and archive readback.

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

## Submission status

The exact tested archive was submitted and confirmed on 2026-09-21. Its SHA-256
matches the archive downloaded back from CRAN incoming/pretest. Acceptance and
publication remain pending CRAN processing. No duplicate submission should be
made while one is pending. The installed v1.0.1 package and original checkout
edits remain unchanged.

## Primary guidance

- [CRAN policies](https://cran.r-project.org/web/packages/policies.html)
- [Submission checklist](https://stat.ethz.ch/CRAN/web/packages/submission_checklist.html)
- [Windows builder](https://win-builder.r-project.org/)
- [Mac Builder](https://mac.r-project.org/macbuilder/submit.html)
