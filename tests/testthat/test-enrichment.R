test_that("enrich_response preserves unified_app_id from original app_id", {
  skip("enrich_response is an internal function - testing through st_top_charts instead")
  
  # Check that unified_app_id was created from the original app_id
  expect_true("unified_app_id" %in% names(enriched),
              "unified_app_id should be created during enrichment")
  
  # Check that the unified_app_id values are the original hex IDs
  expect_equal(unique(enriched$unified_app_id),
               c("5a998ab0ae1f4d57ea38e0f8", "5919fd19ee05f407440011a7"),
               "unified_app_id should preserve the original hex app_id values")
  
  # Check that entities.app_id contains the platform-specific IDs
  expect_true("entities.app_id" %in% names(enriched) || "platform_app_id" %in% names(enriched),
              "Platform-specific IDs should be preserved separately")
})

test_that("enrich_response handles missing app_id gracefully", {
  skip("enrich_response is an internal function - testing through st_top_charts instead")
})

test_that("deduplication consolidates apps with same unified_app_id", {
  skip_if_no_auth()
  
  # Create a scenario where we have duplicate unified_app_ids
  # This simulates what happens after unnesting
  mock_duplicated_data <- data.frame(
    unified_app_id = c("5a998ab0ae1f4d57ea38e0f8", "5a998ab0ae1f4d57ea38e0f8"),
    unified_app_name = c("Bible Word Puzzle", "Bible Word Puzzle"),
    platform_app_id = c("1342112505", "bible.wordgame.words.connect.crossword.cookies"),
    dau_30d_us = c(165000, 165000),
    downloads_30d_ww = c(1350000, 1350000),
    revenue_30d_ww = c(19200, 19200),
    stringsAsFactors = FALSE
  )
  
  # Apply deduplication using internal function with namespace
  deduplicated <- sensortowerR:::deduplicate_by_group_id(mock_duplicated_data, "unified_app_id")
  
  # Should have only one row
  expect_equal(nrow(deduplicated), 1)
  
  # DAU should be averaged (not summed) for duplicates
  expect_equal(deduplicated$dau_30d_us, 165000)
  
  # Downloads should be summed for duplicates
  expect_equal(deduplicated$downloads_30d_ww, 2700000)
})

test_that("st_top_charts handles all scenarios correctly", {
  skip_if_no_auth()
  
  # Test multiple scenarios to ensure robustness
  scenarios <- list(
    list(
      name = "Word games with custom filter",
      params = list(
        os = "unified",
        category = 0,
        custom_fields_filter_id = "603697f4241bc16eb8570d37",
        custom_tags_mode = "include_unified_apps",
        measure = "DAU",
        regions = "US",
        limit = 5
      )
    ),
    list(
      name = "Regular category without custom filter",
      params = list(
        os = "unified",
        category = 7003,  # Puzzle games
        measure = "revenue",
        regions = "US",
        limit = 5
      )
    )
  )
  
  for (scenario in scenarios) {
    result <- do.call(st_top_charts, c(
      scenario$params,
      list(enrich_response = TRUE, deduplicate_apps = TRUE)
    ))
    
    # Basic checks
    expect_s3_class(result, "data.frame")
    
    expect_true("unified_app_id" %in% names(result),
                sprintf("Scenario '%s' should have unified_app_id column", scenario$name))
    
    # Check for duplicates
    dup_count <- sum(duplicated(result$unified_app_id))
    expect_equal(dup_count, 0,
                 sprintf("Scenario '%s' should have no duplicate unified_app_ids", scenario$name))
  }
})