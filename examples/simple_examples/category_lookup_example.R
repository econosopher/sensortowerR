# Example: Using the comprehensive category data

# Load required packages
library(dplyr)
library(sensortowerR)

# Since the data is internal, we need to load it differently
# For now, let's reload it from the source
library(jsonlite)
category_json <- fromJSON("data/raw/category_ids.json")

# Convert to tibbles
ios_categories <- tibble::tibble(
  platform = "ios",
  category_id = names(category_json$category_ids$ios),
  category_name = unlist(category_json$category_ids$ios)
)

android_categories <- tibble::tibble(
  platform = "android", 
  category_id = names(category_json$category_ids$android),
  category_name = unlist(category_json$category_ids$android)
)

st_category_data <- bind_rows(ios_categories, android_categories)

# View all iOS game categories
ios_games <- st_category_data %>%
  filter(platform == "ios", grepl("^7", category_id)) %>%
  arrange(category_id)

cat("iOS Game Categories:\n")
print(ios_games, n = 20)

# View all Android game categories  
android_games <- st_category_data %>%
  filter(platform == "android", grepl("^game", category_id)) %>%
  arrange(category_id)

cat("\n\nAndroid Game Categories:\n")
print(android_games, n = 20)

# Look up specific category names
cat("\n\nLooking up specific categories:\n")

# iOS categories
ios_ids <- c("6014", "7006", "7014")
for (id in ios_ids) {
  name <- st_category_data %>%
    filter(platform == "ios", category_id == id) %>%
    pull(category_name)
  cat(sprintf("iOS %s: %s\n", id, name))
}

# Android categories  
android_ids <- c("game", "game_casino", "game_role_playing")
for (id in android_ids) {
  name <- st_category_data %>%
    filter(platform == "android", category_id == id) %>%
    pull(category_name)
  cat(sprintf("Android %s: %s\n", id, name))
}

# Find a category by name
cat("\n\nFinding categories by name:\n")

# Find all categories with "Casino" in the name
casino_categories <- st_category_data %>%
  filter(grepl("Casino", category_name, ignore.case = TRUE))

print(casino_categories)

# Monopoly Go example - it's in Games (6014), but we can find Casino subcategory
cat("\n\nMonopoly Go is in 'Games' (6014), but Casino games have their own subcategory:\n")
cat("iOS: 7006 = Games/Casino\n")
cat("Android: game_casino = Casino\n")