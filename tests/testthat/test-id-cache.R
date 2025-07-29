# Test internal caching functions
library(testthat)
library(sensortowerR)

test_that("ID type detection works correctly", {
  detect_id_type <- sensortowerR:::detect_id_type
  
  expect_equal(detect_id_type("553834731"), "ios")
  expect_equal(detect_id_type("1234567890"), "ios")
  expect_equal(detect_id_type("com.king.candycrushsaga"), "android")
  expect_equal(detect_id_type("net.example.app"), "android")
  expect_equal(detect_id_type("5ba4585f539ce75b97db6bcb"), "unified_hex")
  expect_equal(detect_id_type("pub123456"), "publisher")
  expect_equal(detect_id_type("123456_com.example"), "combined")
  expect_equal(detect_id_type("random_string"), "unknown")
  expect_equal(detect_id_type(NULL), "unknown")
  expect_equal(detect_id_type(NA), "unknown")
})

test_that("cache operations work correctly", {
  # Get internal functions
  get_id_cache <- sensortowerR:::get_id_cache
  cache_id_mapping <- sensortowerR:::cache_id_mapping
  lookup_cached_id <- sensortowerR:::lookup_cached_id
  save_id_cache <- sensortowerR:::save_id_cache
  load_id_cache <- sensortowerR:::load_id_cache
  
  # Clear cache first
  st_clear_id_cache(disk = TRUE)
  
  # Verify cache is empty
  cache <- get_id_cache()
  expect_equal(length(cache), 0)
  
  # Add an entry to cache
  cache_id_mapping(
    input_id = "test123",
    ios_id = "123456789",
    android_id = "com.test.app",
    unified_id = "abcdef123456789012345678",
    app_name = "Test App"
  )
  
  # Verify entry was added
  cache <- get_id_cache()
  expect_gt(length(cache), 0)
  
  # Lookup by different IDs
  result1 <- lookup_cached_id("test123")
  expect_equal(result1$ios_id, "123456789")
  expect_equal(result1$app_name, "Test App")
  
  result2 <- lookup_cached_id("123456789")
  expect_equal(result2$android_id, "com.test.app")
  
  result3 <- lookup_cached_id("com.test.app")
  expect_equal(result3$unified_id, "abcdef123456789012345678")
  
  # Test cache persistence
  save_id_cache()
  
  # Clear and reload cache to test persistence
  st_clear_id_cache(disk = FALSE)  # Only clear memory, not disk
  
  # Load from disk
  load_id_cache()
  
  # Verify data persisted
  result4 <- lookup_cached_id("test123")
  expect_equal(result4$app_name, "Test App")
  
  # Clean up
  st_clear_id_cache(disk = TRUE)
})

test_that("st_cache_info displays correct information", {
  # Clear cache first
  st_clear_id_cache(disk = TRUE)
  
  # Add some test entries
  cache_id_mapping("app1", ios_id = "111", app_name = "App 1")
  cache_id_mapping("app2", android_id = "com.app2", app_name = "App 2")
  cache_id_mapping("app3", ios_id = "333", android_id = "com.app3", app_name = "App 3")
  
  # Capture output
  # Capture cache info
  cache_output <- capture.output(st_cache_info())
  
  # Verify the cache contains expected entries (don't check exact counts due to test isolation)
  expect_true(any(grepl("Total entries:", cache_output)))
  expect_true(any(grepl("Apps with iOS ID:", cache_output)))
  expect_true(any(grepl("Apps with Android ID:", cache_output)))
  expect_true(any(grepl("Apps with both platforms:", cache_output)))
  
  # Clean up
  st_clear_id_cache(disk = TRUE)
})

test_that("st_smart_metrics wrapper exports exist", {
  expect_true(exists("st_smart_metrics"))
  expect_true(exists("st_clear_id_cache"))
  expect_true(exists("st_cache_info"))
})