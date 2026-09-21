# Local validation

GitHub Actions is not required or used. Run checks on the machine where the
package is being developed:

```sh
Rscript --vanilla tools/check.R
```

This runs the offline tests, builds the source package and vignettes, installs
into a temporary check library, and runs R CMD check. It does not replace the
installed user-library package. It needs R, the package dependencies, roxygen2,
pkgload and Pandoc available locally; ordinary checks use no API credentials.

The same command can run on a local Linux or Windows machine with these
dependencies. Platform coverage requires checks on each target system; no hosted
CI subscription is required. No extra environments are provisioned automatically.

Run `SENSORTOWER_RUN_LIVE=true Rscript tools/live-audit.R` separately with a token
available. Live reads are bounded. Filter creation is tested offline only.

## CRAN preflight

Build with a current R-release or R-patched installation, then check the exact
archive with the manual enabled:

```sh
R CMD build .
Rscript --vanilla tools/check-cran.R sensortowerR_2.0.0.tar.gz
```

The preflight script requires rcmdcheck and a working TeX installation. It writes
results and the archive SHA-256 to `audit/cran-local-check.*`. Any errors or
warnings fail the script; notes require review. On the development Mac, the
system TeX installation is Intel-only. A temporary native TinyTeX runtime was
used for the recorded manual check without changing the system TeX installation.

The R project's [Mac Builder](https://mac.r-project.org/macbuilder/submit.html)
and [Windows builder](https://win-builder.r-project.org/) can check the public
source archive without GitHub Actions. Uploading to these builders is testing,
not CRAN publication. Windows results go to the maintainer email in DESCRIPTION;
wait for those results rather than interpreting upload success as a pass.

See `audit/CRAN-READINESS.md` for the current release gates and `cran-comments.md`
for submission notes. Publication status is recorded in the audit ledger.
