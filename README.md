# sensortowerR

<div align="center">
  <img src="images/sensortowerR_sticker.png" alt="sensortowerR Sticker" style="width:50%; height:auto;"/>
</div>

An R package for interfacing with the Sensor Tower API to fetch mobile app analytics data.

## Installation

```r
# Install from GitHub
devtools::install_github("peterparkerspicklepatch/sensortowerR")
```

## Authentication

The package requires a Sensor Tower API authentication token. You can set it in your R environment:

```r
auth_token <- "YOUR_TOKEN_HERE"
```

## Core Functions

- `get_unified_app_info()`: Search for apps and retrieve their basic information
- `fetch_sensor_tower_metrics()`: Fetch metrics for specific apps
- `get_publisher_games()`: Fetch metadata for each game from a specific publisher

## Example Usage

```r
library(sensortowerR)

# Search for Pokemon games
get_unified_app_info(
  term = "Pokemon",
  app_store = "unified",
  entity_type = "app",
  limit = 4,
  auth_token = auth_token
)

# Fetch metrics for a specific app
fetch_sensor_tower_metrics(
  auth_token = auth_token,
  unified_app_id = "602c795c912b51622f233ffe",  # Pokemon GO
  start_date = ymd("2021-09-22"),
  end_date = ymd("2021-09-22")
)

# Fetch metadata for all games from a publisher
get_publisher_games(
  auth_token = auth_token,
  publisher_id = "560c48b48ac350643900b82d" # Supercell
)
```

## Example Workflow
```r
# Load required packages
pacman::p_load(tidyverse, httr, jsonlite, logging, purrr)

# Set authentication token
auth_token <- ""  # Note: This token should be secured

# Source the package functions
# Find Pokemon games
top_pokemon_games <- get_unified_app_info(
  term = "Pokemon",
  app_store = "unified",
  entity_type = "app",
  limit = 4, # number of results
  auth_token = auth_token
)

# Fetch metrics for Pokemon GO specifically
metrics <- fetch_sensor_tower_metrics(
  auth_token = auth_token,
  unified_app_id = "602c795c912b51622f233ffe", # Pokemon GO app ID
  start_date = ymd("2021-09-22"),
  end_date = ymd("2021-09-22")
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
    end_date = launch_date + days(15)
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

combined_metrics %>%
  mutate(
    date = as.Date(date),
    days_since_launch = as.numeric(date - launch_date),
    country = factor(country)
  ) %>%
  filter(country %in% c("US", "JP")) %>%  # Filter for US and Japan
  group_by(unified_app_name, country, days_since_launch) %>%
  summarize(
    downloads = sum(unified_units),
    revenue = sum(unified_revenue),
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
  facet_grid(metric~country, scales = "free") +
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
```

## Dependencies

- tidyverse
- httr
- jsonlite
- logging
- purrr
