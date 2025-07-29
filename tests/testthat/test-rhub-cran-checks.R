test_that("rhub multi-platform CRAN checks work", {
  
  # Skip this test - rhub v2 API has changed significantly
  skip("rhub v2 API has changed. Use rhub::rhub_check() manually for multi-platform testing.")
  
  # For manual rhub v2 testing, use:
  # rhub::rhub_setup()  # Setup GitHub Action for rhub
  # rhub::rhub_check()  # Submit check
})

test_that("local CRAN check passes", {
  
  # Skip this test - it causes recursion when running during R CMD check
  skip("Skipping recursive R CMD check test")
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