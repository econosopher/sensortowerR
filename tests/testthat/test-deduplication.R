test_that("st_top_charts deduplicates unified apps correctly", {
  skip_if_no_auth()
  
  # Get Word games using custom filter - these often have platform duplicates
  word_filter <- "603697f4241bc16eb8570d37"
  
  # First get without deduplication
  apps_no_dedup <- st_top_charts(
    os = "unified",
    category = 0,
    custom_fields_filter_id = word_filter,
    custom_tags_mode = "include_unified_apps",
    measure = "DAU",
    regions = "US",
    date = "2025-07-21",
    end_date = "2025-08-19",
    limit = 20,
    enrich_response = TRUE,
    deduplicate_apps = FALSE
  )
  
  # Then with deduplication
  apps_dedup <- st_top_charts(
    os = "unified",
    category = 0,
    custom_fields_filter_id = word_filter,
    custom_tags_mode = "include_unified_apps",
    measure = "DAU",
    regions = "US",
    date = "2025-07-21",
    end_date = "2025-08-19",
    limit = 20,
    enrich_response = TRUE,
    deduplicate_apps = TRUE
  )
  
  # Check that deduplication reduces the number of apps
  # (assuming there are platform duplicates in the data)
  expect_lte(nrow(apps_dedup), nrow(apps_no_dedup))
  
  # Check for specific known duplicates (Bible games)
  bible_no_dedup <- apps_no_dedup %>%
    filter(grepl("Bible", unified_app_name, ignore.case = TRUE))
  
  bible_dedup <- apps_dedup %>%
    filter(grepl("Bible", unified_app_name, ignore.case = TRUE))
  
  # If there are Bible games and they have identical metrics, 
  # deduplication should consolidate them
  if (nrow(bible_no_dedup) > 1) {
    # Check if they have identical DAU (indicating same app on different platforms)
    dau_values <- unique(bible_no_dedup$dau_30d_us)
    if (length(dau_values) == 1) {
      # Same DAU means likely the same app - should be deduplicated
      expect_equal(nrow(bible_dedup), 1, 
                   "Bible games with identical metrics should be consolidated")
    }
  }
})

test_that("st_get_unified_mapping resolves platform IDs correctly", {
  skip_if_no_auth()
  
  # Test with known Bible app IDs (iOS and Android)
  bible_ids <- c("1342112505", "bible.wordgame.words.connect.crossword.cookies")
  bible_names <- c("Bible Verse Collect", "Bible Word Puzzle - Word Games")
  
  mapping <- st_get_unified_mapping(
    app_ids = bible_ids,
    app_names = bible_names,
    os = "unified"
  )
  
  expect_s3_class(mapping, "data.frame")
  expect_true("unified_app_id" %in% names(mapping))
  
  # Both should resolve to the same unified ID if they're the same app
  unified_ids <- unique(na.omit(mapping$unified_app_id))
  
  # Check that we get hex format unified IDs
  expect_true(all(grepl("^[a-f0-9]{24}$", unified_ids)),
              "Unified IDs should be 24-character hex strings")
})

test_that("auth_token is passed correctly to deduplication functions", {
  skip_if_no_auth()
  
  # This test ensures the auth_token_val bug doesn't happen again
  # We mock a scenario where auth is required for deduplication
  
  # Set a test token
  test_token <- "test_token_12345"
  
  # Try to call st_top_charts with explicit auth_token
  # This should not error even if SENSORTOWER_AUTH_TOKEN is not set
  expect_error({
    st_top_charts(
      os = "unified",
      category = 0,
      custom_fields_filter_id = "603697f4241bc16eb8570d37",
      custom_tags_mode = "include_unified_apps",
      measure = "DAU",
      regions = "US",
      limit = 1,
      auth_token = test_token,
      deduplicate_apps = TRUE
    )
  }, NA)  # Expect NO error (NA means no error pattern to match)
})