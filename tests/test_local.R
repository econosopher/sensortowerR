# -------------------------------
# Load Required Packages
# -------------------------------
# Using pacman to load necessary libraries
pacman::p_load(
  tidyverse,      # For data manipulation and visualization
  httr,           # For HTTP requests
  jsonlite,       # For JSON parsing
  logging,        # For logging
  purrr,          # For functional programming tools
  lubridate,      # For date manipulation
  scales,         # For scaling functions in ggplot2
  forcats         # For factor manipulation
)

# -------------------------------
# Source Individual Function Scripts
# -------------------------------
# Replace the package function calls by sourcing individual R scripts.
# Ensure that these scripts are located in the "R/" directory relative to your working directory.

source("R/get_unified_app_info.R")       # Defines get_unified_app_info()
source("R/fetch_sensor_tower_metrics.R")# Defines fetch_sensor_tower_metrics()
source("R/get_publisher_games.R")        # Defines get_publisher_games()

# -------------------------------
# Set Authentication Token
# -------------------------------
# **Security Note:** It's best practice to secure your authentication tokens.
# Consider using environment variables or secure key storage solutions.

auth_token <- ""  # Replace with your actual token

# -------------------------------
# Fetch Pokémon Games Information
# -------------------------------
top_pokemon_games <- get_unified_app_info(
  term = "Pokemon",
  app_store = "unified",
  entity_type = "app",
  limit = 4,         # Number of results
  auth_token = auth_token
)

# -------------------------------
# Assign Launch Dates to Games
# -------------------------------
top_pokemon_games <- top_pokemon_games %>%
  mutate(
    launch_date = case_when(
      unified_app_name == "Pokémon GO"          ~ ymd("2016-07-06"),
      unified_app_name == "Pokémon UNITE"       ~ ymd("2021-09-22"),
      unified_app_name == "Pokémon TCG Pocket"  ~ ymd("2024-10-30"),
      unified_app_name == "Pokémon Masters EX"  ~ ymd("2020-07-22"),
      TRUE                                      ~ NA_Date_              # Assign NA for any unmatched game_name
    )
  )

# -------------------------------
# Define a Custom Function to Fetch Metrics
# -------------------------------
fetch_metrics <- function(unified_app_id, app_name, launch_date, auth_token) {
  if (is.na(launch_date)) {
    warning("Launch date is missing for App ID ", unified_app_id, ". Skipping.")
    return(tibble())  # Return empty tibble to allow pmap_dfr to continue
  }

  fetch_sensor_tower_metrics(
    auth_token = auth_token,
    unified_app_id = unified_app_id,
    start_date = launch_date,
    end_date = launch_date + days(40)   # Fetch metrics for 60 days post-launch
  ) %>%
    mutate(
      unified_app_id = unified_app_id,
      unified_app_name = app_name,
      launch_date = launch_date
    )
}

# -------------------------------
# Combine Metrics for All Top Pokémon Games
# -------------------------------
combined_metrics <- top_pokemon_games %>%
  pmap_dfr(~ fetch_metrics(..1, ..2, ..3, auth_token))

# -------------------------------
# Adjust Launch Dates for US and Japan Pokémon GO
# -------------------------------
combined_metrics_jpus <- combined_metrics %>%
  mutate(
    launch_date = case_when(
      country == "US" & unified_app_name == "Pokémon GO" ~ as.Date("2016-07-06"),
      country == "JP" & unified_app_name == "Pokémon GO" ~ as.Date("2016-07-21"), # Adjusted Launch Date
      TRUE ~ as.Date(launch_date)
    )
  )

# -------------------------------
# Calculate Days Since Launch
# -------------------------------
combined_metrics_jpus <- combined_metrics_jpus %>%
  mutate(
    date = as.Date(date),
    days_since_launch = as.numeric(date - launch_date),
    country = factor(country)
  ) %>%
  filter(days_since_launch >= 0, country %in% c("US", "JP")) %>%
  arrange(unified_app_name, country, days_since_launch)# Focus on US and Japan)

# -------------------------------
# Determine Maximum Days Since Launch for Pokémon TCG Pocket
# -------------------------------
# Handle the case where no data is available for Pokémon TCG Pocket
max_day_pokemon_tcg_pocket <- combined_metrics_jpus %>%
  filter(unified_app_name == "Pokémon TCG Pocket") %>%
  pull(days_since_launch) %>%
  max(na.rm = TRUE)

# -------------------------------
# Prepare Data for Plotting
# -------------------------------
plot_data <- combined_metrics_jpus %>%
  # Filter data up to the maximum day across all games
  filter(days_since_launch <= max_day_pokemon_tcg_pocket) %>%
  group_by(unified_app_name, country, days_since_launch) %>%
  summarize(
    downloads = sum((unified_units), na.rm = TRUE),
    revenue = sum((unified_revenue), na.rm = TRUE)
  ) %>%
  # Correct cumulative calculations by grouping appropriately
  group_by(unified_app_name, country) %>%
  arrange(days_since_launch) %>%  # Ensure data is ordered
  mutate(
    total_downloads = cumsum(as.numeric(downloads)),
    total_revenue = cumsum(as.numeric(revenue))
  ) %>%
  arrange(unified_app_name, country, days_since_launch)

# Take the cumulative metrics at the maximum day
  filter(days_since_launch == max_day_pokemon_tcg_pocket)
  pivot_longer(
    cols = c(total_downloads, total_revenue),
    names_to = "metric",
    values_to = "value"
  )

# -------------------------------
# Generate the Plot Without Data Labels
# -------------------------------
ggplot(plot_data, aes(x = fct_reorder(unified_app_name, value, na.rm = TRUE), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(metric ~ country, scales = "free") +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = paste("Cumulative Metrics up to Day", max_day_pokemon_tcg_pocket),
    subtitle = "Data for US and Japan up to maximum day since launch of Pokémon TCG Pocket",
    x = "Game",
    y = "Cumulative Value",
    fill = "Metric",
    caption = "Note: Launch date for Pokémon GO in Japan adjusted to July 22, 2016."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    pl