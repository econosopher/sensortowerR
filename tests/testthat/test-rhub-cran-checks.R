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

test_that("package functions exist", {
  # Test basic functionality without installing
  # This checks that the functions are available in the current package environment
  
  # Skip if not in package development environment
  skip_if_not(exists("st_categories", where = asNamespace("sensortowerR")),
              "Not in package environment")
  
  # Test that key functions exist
  expect_true(exists("st_categories", where = asNamespace("sensortowerR")), 
              "st_categories function should be available")
  expect_true(exists("st_top_charts", where = asNamespace("sensortowerR")), 
              "st_top_charts function should be available")
  expect_true(exists("st_game_summary", where = asNamespace("sensortowerR")), 
              "st_game_summary function should be available")
}) 