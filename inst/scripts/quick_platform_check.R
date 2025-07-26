#!/usr/bin/env Rscript

# Quick platform check script - runs basic checks without interaction
# Usage: Rscript inst/scripts/quick_platform_check.R

message("Starting quick cross-platform checks for sensortowerR...")

# Ensure we're in the package root
if (!file.exists("DESCRIPTION")) {
  stop("This script must be run from the package root directory")
}

# Load required packages
if (!requireNamespace("rcmdcheck", quietly = TRUE)) {
  install.packages("rcmdcheck")
}

# Clean up old artifacts
old_files <- list.files(pattern = "*.tar.gz|*.Rcheck", full.names = TRUE)
if (length(old_files) > 0) {
  message("Cleaning up old files...")
  unlink(old_files, recursive = TRUE)
}

# Run local CRAN check
message("\n=== Running local CRAN check ===")
result <- rcmdcheck::rcmdcheck(args = "--as-cran", error_on = "never")

# Display results
message("\n=== Check Results ===")
message("Errors: ", length(result$errors))
message("Warnings: ", length(result$warnings))
message("Notes: ", length(result$notes))

if (length(result$errors) > 0) {
  message("\nERRORS:")
  for (e in result$errors) message("  - ", e)
}

if (length(result$warnings) > 0) {
  message("\nWARNINGS:")
  for (w in result$warnings) message("  - ", w)
}

if (length(result$notes) > 0) {
  message("\nNOTES:")
  for (n in result$notes) message("  - ", n)
}

# Platform test commands
message("\n=== Platform Test Commands ===")
message("\nFor Windows testing:")
message("  devtools::check_win_devel()")
message("  devtools::check_win_release()")
message("  devtools::check_win_oldrelease()")

message("\nFor multi-platform testing (rhub):")
message("  rhub::rhub_setup()  # First time only")
message("  rhub::rhub_check()  # Submit checks")

message("\nFor macOS testing:")
message("  Upload tarball to: https://mac.r-project.org/macbuilder/submit.html")

message("\nFor comprehensive testing:")
message("  source('inst/scripts/check_all_platforms.R')")
message("  run_all_platform_checks()")

# Exit status based on errors
quit(status = length(result$errors))