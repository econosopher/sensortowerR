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

test_that("unified_app_id is preserved during enrichment", {
  skip_if_no_auth()
  
  # Test that unified_app_id is created and preserved through enrichment
  result <- st_top_charts(
    os = "unified",
    category = 0,
    custom_fields_filter_id = "603697f4241bc16eb8570d37",
    custom_tags_mode = "include_unified_apps",
    measure = "DAU",
    regions = "US",
    limit = 5,
    enrich_response = TRUE,
    deduplicate_apps = TRUE
  )
  
  # Check that unified_app_id exists
  expect_true("unified_app_id" %in% names(result),
              "unified_app_id column must exist after enrichment")
  
  # Check that unified_app_id values are hex format
  non_na_ids <- result$unified_app_id[!is.na(result$unified_app_id)]
  expect_true(all(grepl("^[a-f0-9]{24}$", non_na_ids)),
              "All unified_app_id values should be 24-character hex strings")
  
  # Check that there are no duplicate unified_app_ids
  expect_equal(anyDuplicated(result$unified_app_id), 0,
               "No duplicate unified_app_id values should exist after deduplication")
})

test_that("enrichment process doesn't create duplicates", {
  skip_if_no_auth()
  
  # Get data with enrichment
  enriched <- st_top_charts(
    os = "unified",
    category = 0,
    custom_fields_filter_id = "603697f4241bc16eb8570d37",
    custom_tags_mode = "include_unified_apps",
    measure = "DAU",
    regions = "US",
    limit = 10,
    enrich_response = TRUE,
    deduplicate_apps = TRUE
  )
  
  # Check specific apps that previously had issues
  problem_apps <- c("Zen Word", "Crossword Puzzle Redstone", "Bible Word Puzzle")
  
  for (app_pattern in problem_apps) {
    matching_apps <- enriched %>%
      filter(grepl(app_pattern, unified_app_name, ignore.case = TRUE))
    
    if (nrow(matching_apps) > 0) {
      expect_equal(nrow(matching_apps), 1,
                   sprintf("%s should appear exactly once after deduplication", app_pattern))
    }
  }
})

test_that("deduplication handles CJK characters correctly", {
  skip_if_no_auth()
  
  # Test with Japan market which often has games with CJK characters
  jp_games <- st_top_charts(
    os = "unified",
    category = 7014,  # RPG category
    measure = "revenue",
    regions = "JP",
    limit = 20,
    enrich_response = TRUE,
    deduplicate_apps = TRUE
  )
  
  # Check for CJK characters in names
  cjk_pattern <- "[\\u4e00-\\u9fff\\u3040-\\u309f\\u30a0-\\u30ff\\uac00-\\ud7af]"
  games_with_cjk <- jp_games %>%
    filter(grepl(cjk_pattern, unified_app_name))
  
  # If we have games with CJK characters, check for duplicates
  if (nrow(games_with_cjk) > 0) {
    duplicates <- games_with_cjk %>%
      group_by(unified_app_id) %>%
      filter(n() > 1)
    
    expect_equal(nrow(duplicates), 0,
                 "Games with CJK characters should not have duplicates")
  }
  
  # Check overall for duplicates
  expect_equal(anyDuplicated(jp_games$unified_app_id), 0,
               "No duplicates should exist in Japan market data")
  
  # Test specific known games that might have CJK variants
  known_games <- c("Genshin", "Honkai", "原神", "崩坏")
  for (pattern in known_games) {
    matching <- jp_games %>%
      filter(grepl(pattern, unified_app_name, ignore.case = TRUE))
    
    if (nrow(matching) > 1) {
      # Multiple matches should have different unified_app_ids
      # (different games, not duplicates of the same game)
      unique_ids <- length(unique(matching$unified_app_id))
      expect_equal(unique_ids, nrow(matching),
                   sprintf("Games matching '%s' should have unique IDs", pattern))
    }
  }
})

test_that("auth_token is passed correctly to deduplication functions", {
  skip_if_no_auth()
  
  # This test ensures the auth_token_val bug doesn't happen again
  # We mock a scenario where auth is required for deduplication
  
  # Set a test token
  test_token <- "test_token_12345"
  
  # This test would ideally check that auth_token is passed correctly,
  # but we can't test with a fake token. Instead, ensure the variable
  # name issue (auth_token vs auth_token_val) doesn't recur.
  
  # Check that the st_top_charts function uses auth_token_val consistently
  func_body <- deparse(body(st_top_charts))
  
  # Look for the line where st_get_unified_mapping is called
  mapping_call_lines <- grep("st_get_unified_mapping", func_body, value = TRUE)
  
  if (length(mapping_call_lines) > 0) {
    # Check that auth_token = auth_token_val (not auth_token = auth_token)
    auth_token_lines <- grep("auth_token\\s*=", mapping_call_lines, value = TRUE)
    
    for (line in auth_token_lines) {
      # Make sure we're not passing auth_token = auth_token
      expect_false(grepl("auth_token\\s*=\\s*auth_token[^_]", line),
                   "auth_token should be set to auth_token_val, not auth_token")
    }
  }
})