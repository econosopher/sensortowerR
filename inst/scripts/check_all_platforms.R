#!/usr/bin/env Rscript

# Cross-platform CRAN check script for sensortowerR
# This script helps run CRAN checks across multiple platforms

# Load required packages
required_packages <- c("devtools", "rcmdcheck", "rhub", "pkgbuild")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Installing %s...", pkg))
    install.packages(pkg)
  }
}

# Function to display separator
separator <- function(title) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n", sep = "")
  cat(title, "\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n", sep = "")
}

# Main function to coordinate all checks
run_all_platform_checks <- function() {
  
  cat("SENSORTOWER R PACKAGE - CROSS-PLATFORM CRAN CHECKS\n")
  cat("==================================================\n\n")
  
  # 1. Clean up old build artifacts
  separator("CLEANUP")
  old_tarballs <- list.files(pattern = "*.tar.gz", full.names = TRUE)
  if (length(old_tarballs) > 0) {
    cat("Removing old tarballs:\n")
    for (tb in old_tarballs) {
      cat("  -", tb, "\n")
      unlink(tb)
    }
  }
  
  # 2. Build the package
  separator("BUILD PACKAGE")
  cat("Building package...\n")
  pkg_path <- pkgbuild::build(quiet = FALSE)
  cat("\nPackage built:", pkg_path, "\n")
  
  # 3. Local CRAN check
  separator("LOCAL CRAN CHECK")
  cat("Running R CMD check --as-cran\n\n")
  
  check_result <- rcmdcheck::rcmdcheck(
    path = pkg_path,
    args = "--as-cran",
    error_on = "never"
  )
  
  cat("\nResults:\n")
  cat("- Errors:  ", length(check_result$errors), "\n")
  cat("- Warnings:", length(check_result$warnings), "\n")
  cat("- Notes:   ", length(check_result$notes), "\n")
  
  # 4. rhub checks
  separator("RHUB CHECKS (Multiple Platforms)")
  
  if (requireNamespace("rhub", quietly = TRUE)) {
    cat("rhub v2 is available for cross-platform testing.\n\n")
    cat("To run rhub checks:\n")
    cat("1. First time setup: rhub::rhub_setup()\n")
    cat("2. Submit checks:    rhub::rhub_check()\n\n")
    cat("This will test on:\n")
    cat("  - Ubuntu Linux (multiple R versions)\n")
    cat("  - Windows (multiple R versions)\n")
    cat("  - macOS (multiple R versions)\n")
    cat("  - Debian Linux\n")
    cat("  - Fedora Linux\n\n")
    
    # Ask user if they want to run rhub
    if (interactive()) {
      response <- readline("Do you want to submit to rhub now? (y/n): ")
      if (tolower(response) == "y") {
        rhub::rhub_check()
      }
    }
  }
  
  # 5. Win-builder checks
  separator("WIN-BUILDER CHECKS")
  cat("Win-builder tests on Windows with different R versions.\n\n")
  cat("Available commands:\n")
  cat("  devtools::check_win_devel()      # R-devel\n")
  cat("  devtools::check_win_release()    # R-release\n")
  cat("  devtools::check_win_oldrelease() # R-oldrel\n\n")
  
  if (interactive()) {
    response <- readline("Submit to win-builder? (y/n): ")
    if (tolower(response) == "y") {
      which_version <- readline("Which version? (devel/release/oldrelease/all): ")
      
      if (which_version %in% c("devel", "all")) {
        cat("\nSubmitting to win-builder R-devel...\n")
        devtools::check_win_devel()
      }
      if (which_version %in% c("release", "all")) {
        cat("\nSubmitting to win-builder R-release...\n")
        devtools::check_win_release()
      }
      if (which_version %in% c("oldrelease", "all")) {
        cat("\nSubmitting to win-builder R-oldrelease...\n")
        devtools::check_win_oldrelease()
      }
    }
  }
  
  # 6. macOS builder
  separator("MACOS BUILDER")
  cat("macOS builder tests on real Mac hardware.\n")
  cat("Submit package at: https://mac.r-project.org/macbuilder/submit.html\n")
  cat("Upload the tarball:", pkg_path, "\n\n")
  
  # 7. Additional platform-specific notes
  separator("PLATFORM-SPECIFIC TESTING NOTES")
  
  cat("SOLARIS:\n")
  cat("  - No longer required for CRAN (Solaris support ended)\n\n")
  
  cat("LINUX DISTRIBUTIONS:\n") 
  cat("  - rhub covers major distributions\n")
  cat("  - Consider Docker for specific distro testing\n\n")
  
  cat("ARCHITECTURES:\n")
  cat("  - Most checks cover x86_64\n")
  cat("  - ARM64 (Apple Silicon) covered by macOS builder\n\n")
  
  # 8. Summary
  separator("SUMMARY")
  cat("Package:", pkg_path, "\n\n")
  cat("Checks completed:\n")
  cat("  ✓ Local CRAN check\n")
  cat("  - rhub (if submitted)\n")
  cat("  - win-builder (if submitted)\n")
  cat("  - macOS builder (manual submission required)\n\n")
  
  cat("Next steps:\n")
  cat("1. Review any errors, warnings, or notes\n")
  cat("2. Fix issues and re-run checks\n")
  cat("3. Wait for email results from remote builders\n")
  cat("4. Update cran-comments.md with test results\n\n")
  
  return(invisible(pkg_path))
}

# Run the checks if script is executed directly
if (!interactive() || length(commandArgs(trailingOnly = TRUE)) > 0) {
  run_all_platform_checks()
} else {
  cat("Cross-platform check script loaded.\n")
  cat("Run: run_all_platform_checks()\n")
}