# Install and load required packages
if (!require(devtools)) install.packages("devtools")
devtools::install_github("peterparkerspicklepatch/sensortowerR")
library(sensortowerR)
library(tidyverse)
library(scales)

# Get publisher games using environment variable for auth
auth_token <- Sys.getenv("AUTH_TOKEN")
publisher_id <- "560c48b48ac350643900b82d"

games_df <- get_publisher_games(
    auth_token = auth_token,
    publisher_id = "560c48b48ac350643900b82d"  # Example publisher ID
)

# Show available games
print(head(games_df))

# Define your app information
app_ids_df <- data.frame(
  unified_app_id = c("55c5025102ac64f9c0001f96"),  # Replace with actual app IDs
  unified_app_name = c("Clash of Clans")          # Replace with actual app name
) %>% mutate(
  reference_date = as.Date("2023-01-01"),          # Your chosen reference date
  end_day_offset = reference_date + days(30)                          # 30 days after reference date
)

# Fetch the combined data
metrics <- fetch_sensor_tower_metrics(
  auth_token = auth_token,
  app_id = app_ids_df$unified_app_id,
  app_name = app_ids_df$unified_app_name,
  start_date = app_ids_df$reference_date,
  end_date = app_ids_df$end_day_offset,
  date_granularity = "daily",
  time_period = "day"
  )


