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
dependencies. A macOS pass establishes macOS coverage only. Linux and Windows
remain unverified until checks actually run on those systems; no hosted CI
subscription is required. No extra environments are provisioned automatically.

Run `SENSORTOWER_RUN_LIVE=true Rscript tools/live-audit.R` separately with a token
available. Live reads are bounded. Filter creation is tested offline only.
