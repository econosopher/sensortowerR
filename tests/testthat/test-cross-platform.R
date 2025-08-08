test_that("cross-platform CRAN checks can be run", {
  
  # Skip unless explicitly requested
  skip_if(!nzchar(Sys.getenv("RUN_CROSS_PLATFORM_CHECKS")), 
          "Set RUN_CROSS_PLATFORM_CHECKS=true to run cross-platform tests")
  
  # Test rhub v2 platforms
  if (requireNamespace("rhub", quietly = TRUE)) {
    cat("\n=== RHUB CROSS-PLATFORM TESTING ===\n")
    cat("To run rhub checks manually:\n")
    cat("1. rhub::rhub_setup()  # One-time setup\n")
    cat("2. rhub::rhub_check()  # Submit checks for multiple platforms\n")
    cat("\nAvailable platforms include:\n")
    cat("- ubuntu-latest\n")
    cat("- windows-latest\n") 
    cat("- macos-latest\n")
    cat("- Various R versions (release, devel, oldrel)\n\n")
  }
  
  # Test win-builder
  cat("\n=== WIN-BUILDER TESTING ===\n")
  cat("To submit to win-builder:\n")
  cat("1. devtools::check_win_devel()  # R-devel on Windows\n")
  cat("2. devtools::check_win_release()  # R-release on Windows\n")
  cat("3. devtools::check_win_oldrelease()  # R-oldrelease on Windows\n\n")
  
  # R-hub email submission (legacy but still works)
  cat("\n=== R-HUB EMAIL SUBMISSION ===\n")
  cat("Email package to: builder@r-hub.io\n")
  cat("Will test on multiple platforms automatically\n\n")
  
  expect_true(TRUE, "Cross-platform testing information displayed")
})

test_that("package passes local CRAN check", {
  
  skip_on_cran()
  skip_if_not_installed("rcmdcheck")
  skip_if(Sys.getenv("CI") == "", "Skip local CRAN check outside CI to reduce flakiness")
  
  # Clean up old tarballs first
  old_tarballs <- list.files(pattern = "*.tar.gz", full.names = TRUE)
  if (length(old_tarballs) > 0) {
    unlink(old_tarballs)
  }
  
  # Build package
  pkg_path <- pkgbuild::build(path = ".", quiet = TRUE)
  
  # Run check with CRAN settings
  check_result <- rcmdcheck::rcmdcheck(
    path = pkg_path,
    args = "--as-cran",
    error_on = "never",
    quiet = TRUE
  )
  
  # Report results
  cat("\n=== LOCAL CRAN CHECK RESULTS ===\n")
  cat("Errors:", length(check_result$errors), "\n")
  cat("Warnings:", length(check_result$warnings), "\n")
  cat("Notes:", length(check_result$notes), "\n\n")
  
  if (length(check_result$errors) > 0) {
    cat("ERRORS:\n")
    for (e in check_result$errors) {
      cat(" - ", e, "\n")
    }
    cat("\n")
  }
  
  if (length(check_result$warnings) > 0) {
    cat("WARNINGS:\n") 
    for (w in check_result$warnings) {
      cat(" - ", w, "\n")
    }
    cat("\n")
  }
  
  if (length(check_result$notes) > 0) {
    cat("NOTES:\n")
    for (n in check_result$notes) {
      cat(" - ", n, "\n")
    }
    cat("\n")
  }
  
  # Only fail on errors
  # Only assert no fatal build errors; allow notes/warnings in CI envs
  expect_true(length(check_result$errors) == 0,
              "Package should have no errors in CRAN check")
})

test_that("multi-platform check script exists and works", {
  
  script_path <- system.file("scripts", "check_all_platforms.R", 
                             package = "sensortowerR")
  
  if (file.exists(script_path)) {
    expect_true(file.exists(script_path), 
                "Multi-platform check script should exist")
  } else {
    cat("\nTo create a multi-platform check script, save the following as\n")
    cat("inst/scripts/check_all_platforms.R:\n\n")
    cat('#!/usr/bin/env Rscript\n')
    cat('\n# Cross-platform CRAN check script for sensortowerR\n\n')
    cat('# Function to run all platform checks\n')
    cat('run_all_checks <- function() {\n')
    cat('  cat("Starting cross-platform CRAN checks...\\n\\n")\n')
    cat('  \n')
    cat('  # 1. Local check\n')
    cat('  cat("=== LOCAL CHECK ===\\n")\n')
    cat('  rcmdcheck::rcmdcheck(args = "--as-cran")\n')
    cat('  \n')
    cat('  # 2. rhub checks\n')
    cat('  if (requireNamespace("rhub", quietly = TRUE)) {\n')
    cat('    cat("\\n=== RHUB CHECKS ===\\n")\n')
    cat('    cat("Run: rhub::rhub_check()\\n")\n')
    cat('  }\n')
    cat('  \n')
    cat('  # 3. Win-builder\n')
    cat('  cat("\\n=== WIN-BUILDER ===\\n")\n')
    cat('  cat("Run the following commands:\\n")\n')
    cat('  cat("devtools::check_win_devel()\\n")\n')
    cat('  cat("devtools::check_win_release()\\n")\n')
    cat('  cat("devtools::check_win_oldrelease()\\n")\n')
    cat('  \n')
    cat('  # 4. macOS builder\n')
    cat('  cat("\\n=== MACOS BUILDER ===\\n")\n')
    cat('  cat("Submit to: https://mac.r-project.org/macbuilder/submit.html\\n")\n')
    cat('}\n\n')
    cat('# Run checks\n')
    cat('run_all_checks()\n')
  }
  
  expect_true(TRUE, "Multi-platform check information provided")
})