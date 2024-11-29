# Load required packages
pacman::p_load(tidyverse, httr, jsonlite, logging, purrr)

# Set authentication token
auth_token <- ""  # Note: This token should be secured

# Source the package functions
source("R/get_unified_app_info.R")
# Find Pokemon games
top_pokemon_games <- get_unified_app_info(
  term = "Pokemon",
  app_store = "unified",
  entity_type = "app",
  limit = 4, # number of results
  auth_token = auth_token
)

source("R/fetch_sensor_tower_metrics.R")
# Fetch metrics for Pokemon GO specifically
metrics <- fetch_sensor_tower_metrics(
  auth_token = auth_token,
  unified_app_id = "602c795c912b51622f233ffe", # Pokemon GO app ID
  start_date = ymd("2021-09-22"),
  end_date = ymd("2021-09-22")
)

source("R/get_publisher_games.R")
get_publisher_games(
  auth_token = auth_token,
  publisher_id = "560c48b48ac350643900b82d"
)

top_pokemon_games <-
  mutate(top_pokemon_games, launch_date = case_when(
    unified_app_name == "Pokémon GO" ~ ymd("2016-07-06"),
    unified_app_name == "Pokémon UNITE" ~ ymd("2021-09-22"),
    unified_app_name == "Pokémon TCG Pocket" ~ ymd("2024-10-30"),
    unified_app_name == "Pokémon Masters EX" ~ ymd("2020-07-22"),
    TRUE ~ ymd(NA)  # Assign NA for any unmatched game_name
  ))

# Modified fetch_metrics function
fetch_metrics <- function(unified_app_id, app_name, launch_date, auth_token) {
  if (is.na(launch_date)) {
    warning("Launch date is missing for App ID ", unified_app_id, ". Skipping.")
    return(tibble())  # Return empty tibble to allow pmap_dfr to continue
  }

  fetch_sensor_tower_metrics(
    auth_token = auth_token,
    unified_app_id = unified_app_id,
    start_date = launch_date,
    end_date = launch_date + days(60)
  ) %>%
    mutate(
      unified_app_id = unified_app_id,
      unified_app_name = app_name,
      launch_date = launch_date
    )
}

# Use pmap_dfr to iterate and combine results
combined_metrics <- top_pokemon_games %>%
  pmap_dfr(~ fetch_metrics(..1, ..2, ..3, auth_token))

# Update the combined_metrics data with the correct launch dates
combined_metrics_jpus <- combined_metrics %>%
  mutate(
    launch_date = case_when(
      country == "US" & unified_app_name == "Pokémon GO" ~ as.Date("2016-07-06"),
      country == "JP" & unified_app_name == "Pokémon GO" ~ as.Date("2016-07-22"),
      TRUE ~ as.Date(launch_date)  # Handle other countries if necessary
    )
  )

# Create the plot with the updated launch dates and formatted y-axis labels
combined_metrics_jpus %>%
  mutate(
    date = as.Date(date),
    days_since_launch = as.numeric(date - launch_date),
    country = factor(country)
  ) %>%
  filter(country %in% c("US", "JP")) %>%  # Filter for US and Japan
  group_by(unified_app_name, country, days_since_launch) %>%
  summarize(
    downloads = sum(as.integer(unified_units), na.rm = TRUE),
    revenue = sum(as.integer(unified_revenue), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(unified_app_name, country) %>%
  mutate(
    total_downloads = cumsum(as.numeric(downloads)),
    total_revenue = cumsum(as.numeric(revenue))
  ) %>%
  filter(days_since_launch == 15) %>%
  pivot_longer(
    cols = c(total_downloads, total_revenue),
    names_to = "metric",
    values_to = "value"
  ) %>%
  ggplot(aes(x = fct_reorder(unified_app_name, value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(metric ~ country, scales = "free") +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),  # Abbreviate with K, M, etc.
    expand = expansion(mult = c(0, 0.05))  # Add a little space above the tallest bar
  ) +
  labs(
    title = "Cumulative Metrics on the 15th Day",
    x = "Game",
    y = "Cumulative Value",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
