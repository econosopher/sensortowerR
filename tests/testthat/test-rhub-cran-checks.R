test_that("rhub multi-platform CRAN checks work", {
  
  # Skip this test by default unless explicitly requested
  skip_if_not(Sys.getenv("RUN_RHUB_TESTS") == "true", 
              "Set RUN_RHUB_TESTS=true to run rhub multi-platform checks")
  
  # Skip if not on CI or if rhub is not available
  skip_if_not_installed("rhub")
  skip_if_offline()
  
  # Check if rhub is properly configured
  tryCatch({
    rhub::validate_email(email = NULL)  # Check if email is already validated
  }, error = function(e) {
    skip("rhub email not validated. Run rhub::validate_email() first.")
  })
  
  test_that("package passes rhub CRAN checks on multiple platforms", {
    
    # Get the built package
    pkg_path <- list.files(pattern = "*.tar.gz", full.names = TRUE)
    if (length(pkg_path) == 0) {
      # Build the package if not already built
      pkg_path <- pkgbuild::build(".", dest_path = tempdir())
    } else {
      pkg_path <- pkg_path[1]  # Use the first one found
    }
    
    expect_true(file.exists(pkg_path), "Package tarball should exist")
    
    # Run CRAN checks on rhub
    check_result <- rhub::check_for_cran(
      path = pkg_path,
      email = NULL,  # Use validated email
      show_status = FALSE  # Don't show real-time status in tests
    )
    
    expect_s3_class(check_result, "rhub_check")
    
    # Wait for results (with timeout)
    cat("Waiting for rhub CRAN checks to complete...\n")
    start_time <- Sys.time()
    timeout_minutes <- 30
    
    while (!rhub::get_check(check_result)$status %in% c("success", "error", "cancelled")) {
      Sys.sleep(30)  # Check every 30 seconds
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      
      if (elapsed > timeout_minutes) {
        skip(paste("rhub checks timed out after", timeout_minutes, "minutes"))
      }
      
      cat("Still waiting for rhub checks... (", round(elapsed, 1), "min elapsed)\n")
    }
    
    # Get final results
    final_result <- rhub::get_check(check_result)
    
    # Check results
    expect_equal(final_result$status, "success", 
                 info = paste("rhub CRAN checks failed. Check details at:", final_result$web_url))
    
    # Print summary
    cat("\n=== RHUB CRAN CHECK RESULTS ===\n")
    cat("Status:", final_result$status, "\n")
    cat("Platforms tested:", length(final_result$result), "\n")
    cat("Web URL:", final_result$web_url, "\n")
    
    # Check each platform
    for (i in seq_along(final_result$result)) {
      platform_result <- final_result$result[[i]]
      platform_name <- names(final_result$result)[i]
      
      cat("\nPlatform:", platform_name, "\n")
      cat("Status:", platform_result$status, "\n")
      
      # Expect no errors on any platform
      expect_false(grepl("ERROR", platform_result$result, ignore.case = TRUE),
                   info = paste("Errors found on platform:", platform_name))
    }
  })
})

test_that("local CRAN check passes", {
  
  # This test always runs - basic CRAN compliance check
  skip_on_cran()  # Don't run on CRAN itself
  
  # Run local R CMD check
  check_result <- rcmdcheck::rcmdcheck(
    path = ".",
    args = c("--as-cran", "--no-manual"),
    error_on = "error",  # Only error on actual errors, not warnings/notes
    quiet = TRUE
  )
  
  expect_equal(length(check_result$errors), 0, 
               info = paste("R CMD check errors:", paste(check_result$errors, collapse = "\n")))
  
  # Print summary
  cat("\n=== LOCAL CRAN CHECK RESULTS ===\n")
  cat("Errors:", length(check_result$errors), "\n")
  cat("Warnings:", length(check_result$warnings), "\n") 
  cat("Notes:", length(check_result$notes), "\n")
  
  if (length(check_result$warnings) > 0) {
    cat("Warnings:\n")
    for (w in check_result$warnings) {
      cat(" -", w, "\n")
    }
  }
  
  if (length(check_result$notes) > 0) {
    cat("Notes:\n") 
    for (n in check_result$notes) {
      cat(" -", n, "\n")
    }
  }
})

test_that("package can be installed and loaded", {
  
  # Test that package can be properly installed
  pkg_path <- list.files(pattern = "*.tar.gz", full.names = TRUE)
  
  if (length(pkg_path) > 0) {
    temp_lib <- tempfile()
    dir.create(temp_lib)
    
    # Install to temporary library
    install.packages(pkg_path[1], lib = temp_lib, repos = NULL, type = "source", quiet = TRUE)
    
    # Test loading
    expect_true(require("sensortowerR", lib.loc = temp_lib, character.only = TRUE),
                "Package should load successfully after installation")
    
    # Test basic functionality
    expect_true(exists("st_categories"), "st_categories function should be available")
    expect_true(exists("st_top_charts"), "st_top_charts function should be available")
    expect_true(exists("st_game_summary"), "st_game_summary function should be available")
    
    # Clean up
    detach("package:sensortowerR", unload = TRUE)
    unlink(temp_lib, recursive = TRUE)
    
  } else {
    skip("No package tarball found for installation test")
  }
}) 