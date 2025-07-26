# Test script for new functions: st_category_rankings() and st_app_details()

library(sensortowerR)
library(dplyr)

# Test 1: Category Rankings
cat("Testing st_category_rankings()...\n\n")

# Test iOS game rankings
cat("1. Fetching top free iOS games:\n")
ios_games <- st_category_rankings(
  os = "ios",
  category = 6014,  # Games category
  chart_type = "topfreeapplications",
  country = "US",
  limit = 10
)

if (nrow(ios_games) > 0) {
  cat("Success! Found", nrow(ios_games), "apps\n")
  cat("Columns returned:", paste(names(ios_games), collapse = ", "), "\n")
  # Show first few entries with available columns
  print(head(ios_games, 5))
} else {
  cat("No results returned\n")
}

cat("\n2. Fetching top grossing Android games:\n")
android_games <- st_category_rankings(
  os = "android",
  category = "game",  # Android uses string categories
  chart_type = "topgrossing",
  country = "US",
  limit = 10
)

if (nrow(android_games) > 0) {
  cat("Success! Found", nrow(android_games), "apps\n")
  cat("Columns returned:", paste(names(android_games), collapse = ", "), "\n")
  print(head(android_games, 5))
} else {
  cat("No results returned\n")
}

# Test 2: App Details
cat("\n\nTesting st_app_details()...\n\n")

# Get some app IDs from the rankings to test
if (nrow(ios_games) > 0 && "app_id" %in% names(ios_games)) {
  test_app_ids <- head(ios_games$app_id, 3)
  
  cat("3. Fetching details for top 3 iOS games:\n")
  ios_details <- st_app_details(
    app_ids = test_app_ids,
    os = "ios"
  )
  
  if (nrow(ios_details) > 0) {
    cat("Success! Retrieved details for", nrow(ios_details), "apps\n")
    
    # Display key information
    for (i in 1:nrow(ios_details)) {
      cat("\n--- App", i, "---\n")
      cat("Name:", ios_details$app_name[i], "\n")
      cat("Publisher:", ios_details$publisher_name[i], "\n")
      if ("rating" %in% names(ios_details)) {
        cat("Rating:", ios_details$rating[i], "\n")
      }
      if ("description" %in% names(ios_details)) {
        desc <- substr(ios_details$description[i], 1, 100)
        cat("Description:", desc, "...\n")
      }
    }
  } else {
    cat("No details returned\n")
  }
}

# Test with known app IDs
cat("\n4. Fetching details for known apps:\n")
known_apps <- st_app_details(
  app_ids = c("553834731", "1053012308"),  # Candy Crush, Clash Royale
  os = "ios",
  include_developer_contacts = TRUE
)

if (nrow(known_apps) > 0) {
  cat("Success! Retrieved details for", nrow(known_apps), "apps\n")
  print(known_apps %>% select(app_name, publisher_name, rating, price))
  
  # Check if developer contacts were included
  if ("publisher_email" %in% names(known_apps)) {
    cat("\nDeveloper contact info included\n")
  }
} else {
  cat("No details returned\n")
}

# Test error handling
cat("\n\n5. Testing error handling:\n")
tryCatch({
  st_category_rankings(category = NULL)
}, error = function(e) {
  cat("Expected error caught:", e$message, "\n")
})

tryCatch({
  st_app_details(app_ids = character(0))
}, error = function(e) {
  cat("Expected error caught:", e$message, "\n")
})

cat("\nAll tests completed!\n")