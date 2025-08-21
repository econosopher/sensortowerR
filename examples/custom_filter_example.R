# Example: Using Custom Fields Filters with sensortowerR
# Demonstrates the new custom fields functionality

library(sensortowerR)
library(dplyr)
library(ggplot2)

# This example demonstrates how to use the new custom fields filter functions
# to create and use filters programmatically, without needing to use the web interface

cat("=== Custom Fields Filter Examples ===\n\n")

# Example 1: Create and use a simple filter for Word games
tryCatch({
  cat("=== Example 1: Word Games Filter ===\n")
  
  # Method 1: Use st_get_filtered_apps directly
  word_games <- st_get_filtered_apps(
    field_name = "Game Sub-genre",
    field_values = "Word",
    measure = "DAU",
    regions = "US",
    limit = 10,
    enrich_response = TRUE
  )
  
  if (nrow(word_games) > 0) {
    cat("\nTop Word games by DAU:\n")
    print(word_games %>% 
            select(unified_app_name, dau_30d_us, retention_7d_us) %>%
            head(5))
  }
  
  # Method 2: Create filter ID first, then reuse it
  word_filter_id <- st_create_simple_filter(
    field_name = "Game Sub-genre",
    field_values = "Word"
  )
  cat("\nWord games filter ID:", word_filter_id, "\n")
  
}, error = function(e) {
  cat("Error with Word games example:", e$message, "\n")
})

# Example 2: Filter by genre
tryCatch({
  cat("\n=== Example 2: Puzzle Games Filter ===\n")
  
  # Use the utility function for genre filtering
  puzzle_games <- st_get_filtered_apps(
    field_name = "Game Genre",
    field_values = "Puzzle",
    measure = "revenue",
    regions = "US",
    date = Sys.Date() - 30,
    end_date = Sys.Date() - 1,
    limit = 10
  )
  
  if (nrow(puzzle_games) > 0) {
    cat("\nTop Puzzle games by revenue:\n")
    print(puzzle_games %>% 
            select(unified_app_name, revenue_30d_ww, downloads_30d_ww) %>%
            head(5))
  }
  
}, error = function(e) {
  cat("Error with Puzzle games example:", e$message, "\n")
})

# Example 3: Filter by monetization model
tryCatch({
  cat("\n=== Example 3: Free-to-Play with Ads ===\n")
  
  # Find free games with ads
  f2p_ads_games <- st_get_filtered_apps(
    field_name = "Free",
    field_values = list(),  # Boolean field
    measure = "DAU",
    regions = "US",
    limit = 10
  )
  
  if (nrow(f2p_ads_games) > 0) {
    cat("\nTop F2P games with ads:\n")
    print(f2p_ads_games %>% 
            select(unified_app_name, dau_30d_us, revenue_30d_ww) %>%
            head(5))
  }
  
}, error = function(e) {
  cat("Error with F2P example:", e$message, "\n")
})

# Example 4: Discover available fields
tryCatch({
  cat("\n=== Example 4: Discover Available Fields ===\n")
  
  # Find all fields related to "retention"
  retention_fields <- st_discover_fields("retention")
  
  if (nrow(retention_fields) > 0) {
    cat("\nAvailable retention-related fields:\n")
    print(retention_fields %>% 
            select(name, global, value_count) %>%
            head(10))
  }
  
  # Find game-related fields
  game_fields <- st_discover_fields("game")
  cat("\nFound", nrow(game_fields), "game-related fields\n")
  
}, error = function(e) {
  cat("Error discovering fields:", e$message, "\n")
})

# Example 5: Complex filter - High retention puzzle games
tryCatch({
  cat("\n=== Example 5: Complex Filter - High Retention Puzzle Games ===\n")
  
  # Create a complex filter with multiple criteria
  complex_filter <- st_custom_fields_filter(
    custom_fields = list(
      list(
        name = "Game Genre",
        values = list("Puzzle"),
        global = TRUE,
        exclude = FALSE
      ),
      list(
        name = "Free",
        global = TRUE,
        true = TRUE
      ),
      list(
        name = "Day 7 Retention (Last Quarter, US)",
        values = list("25% - 100%"),  # High retention
        global = TRUE,
        exclude = FALSE
      )
    )
  )
  
  cat("Complex filter ID:", complex_filter, "\n")
  
  # Use the complex filter
  high_retention_puzzles <- st_get_filtered_apps(
    filter_id = complex_filter,
    measure = "DAU",
    regions = "US",
    limit = 10
  )
  
  if (nrow(high_retention_puzzles) > 0) {
    cat("\nHigh retention puzzle games:\n")
    print(high_retention_puzzles %>% 
            select(unified_app_name, dau_30d_us, retention_7d_us) %>%
            head(5))
  }
  
}, error = function(e) {
  cat("Error with complex filter:", e$message, "\n")
})

# Example 6: Analyze a filter
tryCatch({
  cat("\n=== Example 6: Analyze Filter Results ===\n")
  
  # Analyze Word games
  word_analysis <- st_analyze_filter(
    filter_id = "603697f4241bc16eb8570d37",  # Word games filter
    measure = "DAU",
    regions = "US",
    top_n = 5
  )
  
  cat("\nWord Games Analysis:\n")
  cat("Total apps:", word_analysis$total_apps, "\n")
  cat("Filter criteria:\n")
  print(word_analysis$filter_criteria)
  cat("\nTop apps:\n")
  print(word_analysis$top_apps %>% 
          select(unified_app_name, dau_30d_us))
  
}, error = function(e) {
  cat("Error analyzing filter:", e$message, "\n")
})

# Example 7: Using filter collections
tryCatch({
  cat("\n=== Example 7: Filter Collections ===\n")
  
  # Get pre-built genre filters
  genres <- st_get_filter_collection("top_genres")
  
  if (!is.null(genres)) {
    cat("\nAvailable genre filters:\n")
    cat(paste(names(genres), collapse = ", "), "\n")
    
    # Use a filter from the collection
    if ("puzzle" %in% names(genres)) {
      puzzle_apps <- st_top_charts(
        os = "unified",
        category = 0,
        custom_fields_filter_id = genres$puzzle,
        custom_tags_mode = "include_unified_apps",
        measure = "DAU",
        regions = "US",
        limit = 5
      )
      
      if (nrow(puzzle_apps) > 0) {
        cat("\nTop puzzle apps from collection:\n")
        print(puzzle_apps %>% 
                select(unified_app_name, dau_30d_us) %>%
                head())
      }
    }
  }
  
}, error = function(e) {
  cat("Error with filter collections:", e$message, "\n")
})

# Example 8: Visualizing filtered data
tryCatch({
  cat("\n=== Example 8: Visualizing Filter Results ===\n")
  
  # Get top Word games
  viz_data <- st_get_filtered_apps(
    field_name = "Game Sub-genre",
    field_values = "Word",
    measure = "DAU",
    regions = "US",
    limit = 10
  )
  
  if (nrow(viz_data) > 0 && "dau_30d_us" %in% names(viz_data)) {
    # Create a bar chart of top Word games
    p <- ggplot(viz_data %>% head(10), 
                aes(x = reorder(unified_app_name, dau_30d_us), 
                    y = dau_30d_us/1e6)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Word Games by Daily Active Users",
           subtitle = "US Market - Last 30 Days Average",
           x = "",
           y = "DAU (Millions)") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 10, color = "gray50"))
    
    print(p)
    
    # Save the plot
    ggsave("word_games_dau_chart.png", p, width = 10, height = 6, dpi = 300)
    cat("\nChart saved as 'word_games_dau_chart.png'\n")
  }
  
}, error = function(e) {
  cat("Error with visualization:", e$message, "\n")
})

# Important notes
cat("\n=== Important Notes ===\n")
cat("1. Custom fields filters are more flexible than the old custom_fields_filter_id\n")
cat("2. You can create filters programmatically without using the web interface\n")
cat("3. Filter IDs are permanent and can be reused across sessions\n")
cat("4. The same filter criteria always returns the same filter ID\n")
cat("5. Use st_discover_fields() to explore available filtering options\n")
cat("6. Complex filters can combine multiple criteria with AND logic\n")
cat("7. Use filter collections for common filtering scenarios\n")

# Comparison with old method
cat("\n=== Old Method vs New Method ===\n")
cat("Old: Create filter in web UI -> Copy ID from URL -> Use in API\n")
cat("New: Define filter criteria in R -> Create/reuse filter ID -> Use directly\n")
cat("\nThe new method is more programmatic and doesn't require web UI access!\n")