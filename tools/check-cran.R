# Check the exact source archive intended for submission, including its manual.
# Run R CMD build first. This does not upload or submit the package.
args <- commandArgs(trailingOnly = TRUE)
desc <- read.dcf("DESCRIPTION")
archive <- if (length(args)) args[[1]] else paste0(desc[1, "Package"], "_", desc[1, "Version"], ".tar.gz")
if (!file.exists(archive)) stop("Build the source archive with R CMD build first.")
Sys.setenv(SENSORTOWER_AUTH_TOKEN = "", SENSORTOWER_RUN_LIVE = "false", NOT_CRAN = "false")
result <- rcmdcheck::rcmdcheck(
  path = archive, args = "--as-cran", check_dir = tempfile("sensortower-cran-"),
  error_on = "never", quiet = FALSE
)
dir.create("audit", showWarnings = FALSE)
writeLines(sub("[\r\n]+$", "", result$stdout), "audit/cran-local-check.log")
jsonlite::write_json(list(
  checked_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  archive = basename(archive),
  sha256 = paste0(openssl::sha256(file(archive, "rb"))),
  r = R.version.string, platform = R.version$platform,
  errors = result$errors, warnings = result$warnings, notes = result$notes
), "audit/cran-local-check.json", pretty = TRUE, auto_unbox = TRUE)
if (length(result$errors) || length(result$warnings)) stop("CRAN preflight has errors or warnings; see audit/cran-local-check.log.")
if (length(result$notes)) message("Review all notes before submission; see audit/cran-local-check.json.")
