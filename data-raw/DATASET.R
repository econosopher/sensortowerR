# Load required packages
library(jsonlite)
library(tibble)
library(dplyr)

# Load category data from JSON file
category_json <- fromJSON("data/raw/category_ids.json")

# Convert iOS categories to tibble
ios_categories <- tibble(
  platform = "ios",
  category_id = names(category_json$category_ids$ios),
  category_name = unlist(category_json$category_ids$ios)
)

# Convert Android categories to tibble
android_categories <- tibble(
  platform = "android",
  category_id = names(category_json$category_ids$android),
  category_name = unlist(category_json$category_ids$android)
)

# Combine into single dataset
st_category_data <- bind_rows(ios_categories, android_categories)

# Games breakdown API response key for field interpretation
games_breakdown_key <- list(
  ios = list(
    aid = "App ID",
    cc = "Country Code", 
    d = "Date",
    iu = "iPhone Downloads",
    ir = "iPhone Revenue",
    au = "iPad Downloads",
    ar = "iPad Revenue"
  ),
  android = list(
    aid = "App ID",
    cc = "Country Code",
    d = "Date", 
    u = "Android Downloads",
    r = "Android Revenue"
  )
)

# Sales report estimates API response key for field interpretation
# Note: All revenues are returned in cents
sales_report_key <- list(
  ios = list(
    aid = "App ID",
    cc = "Country Code",
    d = "Date",
    iu = "iPhone Downloads",
    ir = "iPhone Revenue (cents)",
    au = "iPad Downloads", 
    ar = "iPad Revenue (cents)"
  ),
  android = list(
    aid = "App ID",
    cc = "Country Code",
    d = "Date",
    u = "Android Downloads",
    r = "Android Revenue (cents)"
  )
)

# API recommendations for date segmentation to avoid timeouts
api_date_recommendations <- list(
  daily = "1 week segments",
  weekly = "3 month segments",
  monthly = "1 year segments",
  quarterly = "2 year segments"
)

# Save all data objects
usethis::use_data(
  st_category_data, 
  games_breakdown_key, 
  sales_report_key,
  api_date_recommendations,
  internal = TRUE, 
  overwrite = TRUE
)