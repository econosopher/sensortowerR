# CRAN readiness: 2.0.0 candidate

The candidate is pushed on `codex/sensortower-v2`. GitHub Actions remains
disabled. The package has not been submitted to CRAN or installed over v1.0.1.

## Verified on 2026-09-21

| Check | Result | Evidence |
|---|---|---|
| Offline tests | 37 tests, 191 assertions pass | offline-tests.csv |
| Local macOS R 4.2.2, --as-cran with manual | 0 errors, 0 warnings, 1 environment note: unable to verify current time | cran-local-check.log, cran-local-check.json |
| Mac Builder R 4.6.1 Patched, arm64 | 0 errors, 0 warnings, 0 notes | cran-mac-builder-check.log |
| Documentation URLs | All three passed urlchecker | Local urlchecker::url_check() |
| CRAN reverse dependencies | None, including optional dependencies | cran-reverse-dependencies.json |
| Source archive | 42 KB; no raw live receipts or credential patterns | cran-candidate.json |
| Live API audit | 24 verified cases; legacy retention HTTP 404 | live-audit.csv |

The Mac Builder check included installation, examples, tests, vignette rebuilding
and the PDF manual. Its arguments were `--no-clean-on-error`; the local check
separately exercised `--as-cran`. Builder results:
https://mac.R-project.org/macbuilder/results/1789987201-fa38cc2215b2b034/

The stricter check found an unnecessary data-raw directory in the archive and
an unusable Intel-only TeX installation on the development Mac. The directory is
now excluded. The manual passed with an isolated native TinyTeX runtime, and on
Mac Builder. Runnable offline examples were added. Historical 0.9.7 submission
metadata was moved to the audit directory; submission comments now describe 2.0.

## Remaining gates

1. Receive the Windows R-devel check result. The upload succeeded; no result
   has arrived yet. It is not a verified pass. The builder sends results to
   pblack@gameeconomistconsulting.com.
2. Run a Linux check to satisfy the original three-platform release plan.
   No Linux runtime is installed locally; no paid runners have been enabled.
3. Build the final archive with current R-release or R-patched. The currently
   checked archive was built with R 4.2.2, although current Mac Builder checked
   it successfully. Recheck the exact final archive and record its hash.
4. Update cran-comments.md with final results and resolve significant notes.
5. Submit the source tarball through the CRAN submission form when release is
   approved. CRAN sends a maintainer confirmation email. Do not submit a second
   copy while the first is pending.

The current public CRAN release is 1.0.1; its listed platform checks are all OK.
Those results do not validate 2.0.0. The candidate's exact archive hash and remote
check status are in cran-candidate.json and cran-builder-status.json.

## Primary guidance

- [CRAN policies](https://cran.r-project.org/web/packages/policies.html): current-R source builds, --as-cran checks, reverse dependencies and submission procedure.
- [Submission checklist](https://stat.ethz.ch/CRAN/web/packages/submission_checklist.html): current R-devel checks and Windows builder.
- [Windows builder](https://win-builder.r-project.org/): R-devel upload and result delivery to the maintainer.
- [Mac Builder](https://mac.r-project.org/macbuilder/submit.html): current macOS package checks.
