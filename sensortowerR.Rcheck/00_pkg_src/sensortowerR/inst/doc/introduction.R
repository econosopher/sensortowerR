## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(sensortowerR)
# library(lubridate) # For easy date creation
# 
# # --- Example 1: Get App Info ---
# # Search for apps matching "Clash Royale"
# app_info <- st_app_info(term = "Clash Royale", limit = 1)
# print(app_info)
# # Use the unified_app_id from app_info in other functions if needed
# 
# # --- Example 2: Get Publisher's Apps ---
# # Fetch apps published by Supercell (replace with a valid ID)
# publisher_apps <- st_publisher_apps(publisher_id = "560c48b48ac350643900b82d")
# print(publisher_apps)
# 
# # --- Example 3: Fetch Detailed Metrics for One App ---
# # Fetch metrics for a specific app ID (replace with a valid ID)
# # Requires a valid unified_app_id known beforehand
# metrics <- st_metrics(
#   unified_app_id = "YOUR_VALID_UNIFIED_APP_ID",
#   start_date = Sys.Date() - 15,
#   end_date = Sys.Date() - 1
# )
# print(metrics)
# head(metrics)
# 
# # --- Example 4: Get Top Apps by Downloads ---
# # Get top 5 iOS Games by downloads ("units") in the US for the last full month
# top_downloads <- st_top_sales(
#   os = "ios",
#   comparison_attribute = "absolute", # Or "delta", "transformed_delta"
#   time_range = "month",
#   measure = "units",
#   date = floor_date(Sys.Date() - months(1), "month"), # Start of last month
#   category = 6000, # iOS Games category ID
#   regions = "US",  # Region is required
#   limit = 5
#   # device_type defaults to "total" for iOS
# )
# print(top_downloads)
# 
# # --- Example 5: Get Top Apps by Active Users ---
# # Get top 3 Android apps by MAU worldwide for the last quarter
# top_mau <- st_top_active_users(
#   os = "android",
#   comparison_attribute = "absolute",
#   time_range = "quarter",
#   measure = "MAU",
#   date = floor_date(Sys.Date() - months(3), "quarter"), # Start of last quarter
#   regions = "WW", # Worldwide region is required
#   # category = NULL, # Optional: omit for all categories
#   limit = 3
# )
# print(top_mau)

