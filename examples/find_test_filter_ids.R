# Script to Help Find or Create Test Filter IDs
# This script demonstrates the new custom fields API functions
# for programmatic filter creation without using the web UI

library(sensortowerR)

cat("=== Sensor Tower Custom Filter ID Helper ===\n\n")

# Method 1: Programmatically create filters using new API functions
cat("## Method 1: Create Filters Programmatically (NEW!)\n")
cat("No need to use the web UI anymore!\n\n")

# Create a filter for Word puzzle games
cat("Creating Word puzzle games filter...\n")
word_filter_id <- tryCatch({
  st_create_simple_filter(
    field_name = "Game Sub-genre",
    field_values = "Word"
  )
}, error = function(e) {
  cat("  Error:", e$message, "\n")
  NULL
})

if (!is.null(word_filter_id)) {
  cat("  ✅ Created filter:", word_filter_id, "\n")
  
  # Verify the filter
  details <- st_custom_fields_filter_by_id(word_filter_id)
  cat("  Criteria:", details$custom_fields$name[1], "=", 
      paste(details$custom_fields$values[[1]], collapse = ", "), "\n")
}

# Test the example filter
cat("Testing example filter...\n")
result <- st_test_filter(example_filter, verbose = TRUE)

# Method 2: Discover available fields and values
cat("\n## Method 2: Discover Available Fields\n")
cat("See what fields are available for filtering:\n\n")

# Get game-related fields
cat("Discovering game fields...\n")
game_fields <- tryCatch({
  st_discover_fields("game", show_values = FALSE)
}, error = function(e) NULL)

if (!is.null(game_fields)) {
  cat(sprintf("  Found %d game-related fields\n", nrow(game_fields)))
  cat("  Sample fields:\n")
  print(head(game_fields %>% select(name, type), 5))
}

# Method 3: Use utility functions for common filters
cat("\n## Method 3: Utility Functions for Common Filters\n")

# Genre filter
cat("\nCreating Puzzle genre filter...\n")
puzzle_filter <- tryCatch({
  st_filter_by_genre("Puzzle")
}, error = function(e) NULL)

if (!is.null(puzzle_filter)) {
  cat("  Puzzle filter ID:", puzzle_filter, "\n")
}

# Monetization filter
cat("\nCreating F2P with ads filter...\n")
f2p_filter <- tryCatch({
  st_filter_by_monetization(free_only = TRUE, has_ads = TRUE)
}, error = function(e) NULL)

if (!is.null(f2p_filter)) {
  cat("  F2P with ads filter:", f2p_filter, "\n")
}

# Method 4: Get pre-defined filter collections
cat("\n## Method 4: Pre-defined Filter Collections\n")

# Get top genres collection
cat("Getting top genres collection...\n")
top_genres <- tryCatch({
  st_get_filter_collection("top_genres")
}, error = function(e) NULL)

if (!is.null(top_genres)) {
  cat(sprintf("  Collection contains %d genre filters\n", length(top_genres)))
  cat("  Available genres:", paste(names(top_genres), collapse = ", "), "\n")
}

# Method 5: Extract filter ID from existing URL
cat("\n## Method 5: Extract Filter ID from URL\n")
cat("If you have a Sensor Tower URL with filters, extract the ID:\n\n")

example_url <- "https://app.sensortower.com/top-charts?custom_fields_filter_id=603697f4241bc16eb8570d37"
extracted_id <- st_extract_filter_id(example_url)
if (!is.null(extracted_id)) {
  cat("  Extracted ID:", extracted_id, "\n")
  
  # Get filter details
  details <- tryCatch({
    st_custom_fields_filter_by_id(extracted_id)
  }, error = function(e) NULL)
  
  if (!is.null(details)) {
    cat("  Filter criteria:", details$custom_fields$name[1], "\n")
  }
}

# Method 6: Test and use filters with st_top_charts
cat("\n## Method 6: Using Custom Filters with st_top_charts\n")
cat("Use your filter IDs to get filtered app data:\n\n")

if (!is.null(word_filter_id)) {
  cat("Using Word games filter to get top apps...\n")
  
  top_word_games <- tryCatch({
    st_top_charts(
      category = 0,  # Must be 0 when using custom filters
      custom_fields_filter_id = word_filter_id,
      custom_tags_mode = "include_unified_apps",
      os = "unified",
      regions = "US",
      measure = "DAU",
      limit = 5,
      enrich_response = TRUE,
      deduplicate_apps = TRUE
    )
  }, error = function(e) NULL)
  
  if (!is.null(top_word_games) && nrow(top_word_games) > 0) {
    cat("\nTop 5 Word games by DAU:\n")
    if ("unified_app_name" %in% names(top_word_games)) {
      for (i in 1:min(5, nrow(top_word_games))) {
        cat(sprintf("  %d. %s\n", i, top_word_games$unified_app_name[i]))
      }
    }
  }
}

# Alternative: Use the helper function
cat("\nUsing st_get_filtered_apps helper function...\n")
top_rpgs <- tryCatch({
  st_get_filtered_apps(
    field_name = "Game Genre",
    field_values = "Role Playing",
    measure = "revenue",
    regions = "US",
    limit = 3
  )
}, error = function(e) NULL)

if (!is.null(top_rpgs) && nrow(top_rpgs) > 0) {
  cat("\nTop 3 RPGs by revenue:\n")
  if ("unified_app_name" %in% names(top_rpgs)) {
    for (i in 1:min(3, nrow(top_rpgs))) {
      cat(sprintf("  %d. %s\n", i, top_rpgs$unified_app_name[i]))
    }
  }
}

# Provide instructions for the new approach
cat("\n## New Approach: No Web UI Needed!\n")
cat("With the new custom fields API functions, you can:\n")
cat("1. Discover available fields with st_discover_fields()\n")
cat("2. Create filters programmatically with st_create_simple_filter()\n")
cat("3. Use utility functions for common filters\n")
cat("4. Get filter details with st_custom_fields_filter_by_id()\n")
cat("5. Use filters directly in st_top_charts() and st_get_filtered_apps()\n")
cat("\nNo need to manually create filters in the web UI anymore!\n")

# Save commonly used filters
cat("\n## Saving Your Filter IDs\n")

if (!is.null(word_filter_id) || !is.null(puzzle_filter)) {
  filter_config <- list()
  
  if (!is.null(word_filter_id)) {
    filter_config$word_games <- list(
      id = word_filter_id,
      description = "Word puzzle games",
      created = Sys.Date()
    )
  }
  
  if (!is.null(puzzle_filter)) {
    filter_config$puzzle_genre <- list(
      id = puzzle_filter,
      description = "All puzzle games",
      created = Sys.Date()
    )
  }
  
  # Save as YAML
  if (requireNamespace("yaml", quietly = TRUE)) {
    yaml::write_yaml(filter_config, "my_custom_filters.yml")
    cat("Created filter configuration file: my_custom_filters.yml\n")
    cat("You can reuse these filter IDs in future scripts\n")
  }
}

# Summary
cat("\n=== Summary ===\n")
cat("1. Use st_create_simple_filter() to create filters programmatically\n")
cat("2. Use st_discover_fields() to see available filter criteria\n")
cat("3. Use utility functions for common filter types (genre, monetization, etc.)\n")
cat("4. Filter IDs are stable and can be reused across sessions\n")
cat("5. Always use category=0 when using custom_fields_filter_id\n")
cat("6. Enable deduplicate_apps=TRUE to avoid duplicate entries\n")