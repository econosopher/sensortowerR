test_that("rhub multi-platform CRAN checks work", {
  
  # Skip this test - rhub v2 API has changed significantly
  skip("rhub v2 API has changed. Use rhub::rhub_check() manually for multi-platform testing.")
  
  # For manual rhub v2 testing, use:
  # rhub::rhub_setup()  # Setup GitHub Action for rhub
  # rhub::rhub_check()  # Submit check
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