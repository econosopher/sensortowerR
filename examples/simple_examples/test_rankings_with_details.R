# Test combining st_category_rankings() with st_app_details()
# This demonstrates the design philosophy of combining endpoints for better UX

library(sensortowerR)
library(dplyr)

cat("Fetching top free iOS games and enriching with details...\n\n")

# Step 1: Get rankings
rankings <- st_category_rankings(
  os = "ios",
  category = 6014,  # Games
  chart_type = "topfreeapplications",
  country = "US",
  limit = 10
)

cat("Got", nrow(rankings), "ranked apps\n")
print(rankings)

# Step 2: Get app details for the ranked apps
if (nrow(rankings) > 0) {
  cat("\nFetching app details for top ranked apps...\n")
  
  app_details <- st_app_details(
    app_ids = rankings$app_id,
    os = "ios"
  )
  
  # Step 3: Combine rankings with app details
  enriched_rankings <- rankings %>%
    left_join(
      app_details %>% select(app_id, app_name, publisher_name, rating, price),
      by = "app_id"
    ) %>%
    arrange(rank)
  
  cat("\nTop 10 Free iOS Games with Details:\n")
  print(enriched_rankings %>% 
    select(rank, app_name, publisher_name, rating, app_id) %>%
    as.data.frame())
}

# Demonstrate how this could be wrapped in a convenience function
get_rankings_with_details <- function(os = "ios", 
                                    category, 
                                    chart_type = NULL,
                                    country = "US", 
                                    limit = 20) {
  
  # Get rankings
  rankings <- st_category_rankings(
    os = os,
    category = category,
    chart_type = chart_type,
    country = country,
    limit = limit
  )
  
  if (nrow(rankings) == 0) {
    return(rankings)
  }
  
  # Get app details
  details <- st_app_details(
    app_ids = rankings$app_id,
    os = os
  )
  
  # Combine
  rankings %>%
    left_join(details, by = "app_id") %>%
    arrange(rank) %>%
    select(rank, app_id, app_name, publisher_name, rating, everything())
}

cat("\n\nTesting convenience function for Android games:\n")
android_top <- get_rankings_with_details(
  os = "android",
  category = "game",
  chart_type = "topgrossing",
  limit = 5
)

# Check what columns are available
cat("Available columns:", paste(names(android_top), collapse = ", "), "\n")

# Select available columns
if ("app_name" %in% names(android_top)) {
  print(android_top %>% 
    select(rank, app_name, publisher_name, rating) %>%
    as.data.frame())
} else {
  print(android_top %>% 
    select(rank, app_id, any_of(c("name", "title", "app_name"))) %>%
    head())