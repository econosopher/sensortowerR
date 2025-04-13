Okay, here is the complete updated README content formatted as a single block of Markdown text. You can copy this entire block and paste it directly into your `README.md` file, replacing the old content.

```markdown
# sensortowerR

<div align="center">
  <img src="images/sensortowerR_sticker.png" alt="sensortowerR Sticker" style="width:50%; height:auto;"/>
</div>

An R package for interfacing with the Sensor Tower API to fetch mobile app analytics data, including app info, publisher details, revenue/download estimates, and active user metrics.

## Installation

You need the `devtools` package to install `sensortowerR` from GitHub.

```r
# install.packages("devtools") # Run this if you don't have devtools installed
devtools::install_github("peterparkerspicklepatch/sensortowerR")
```

## Authentication (Important!)

This package requires a Sensor Tower API authentication token to function. **It is strongly recommended to store your token securely as an environment variable rather than hardcoding it in your scripts.**

The functions in this package will automatically look for an environment variable named `SENSORTOWER_AUTH_TOKEN`.

**Recommended Setup:**

1.  Use the `usethis` package to edit your R environment file:
    ```r
    # Run this in your R console
    usethis::edit_r_environ()
    ```
    *(This usually opens the file `~/.Renviron` in your home directory. You can also use `usethis::edit_r_environ(scope = "project")` to create one specific to your current project, but be sure to add `.Renviron` to your `.gitignore` file if you do!)*

2.  Add the following line to the `.Renviron` file that opens, replacing `"YOUR_SECRET_TOKEN_HERE"` with your actual Sensor Tower API token:
    ```
    SENSORTOWER_AUTH_TOKEN="YOUR_SECRET_TOKEN_HERE"
    ```

3.  Save the `.Renviron` file and **restart your R session** for the changes to take effect.

Once set up, you generally won't need to pass the `auth_token` argument to the functions.

## Core Functions

The package provides the following main functions:

-   `get_unified_app_info()`: Search for apps or publishers and retrieve basic information (like unified IDs).
-   `get_publisher_games()`: Fetch metadata for all apps associated with a specific publisher ID.
-   `fetch_sensor_tower_metrics()`: Fetch detailed daily metrics (revenue, downloads, active users) for a specific unified app ID over a date range.
-   `get_top_apps_by_revenue_and_downloads()`: Retrieve ranked lists of top apps based on revenue or downloads ("units") for specific criteria (OS, category, region, time period, etc.). Replaces `st_get_sales_estimates`.
-   `get_top_apps_by_active_users()`: Retrieve ranked lists of top apps based on DAU, WAU, or MAU for specific criteria (OS, category, region, time period, etc.). Replaces `st_get_active_users`.

## Example Usage

Make sure you have set the `SENSORTOWER_AUTH_TOKEN` environment variable and restarted R before running these examples.

```r
library(sensortowerR)
library(lubridate) # For easy date creation

# --- Example 1: Get App Info ---
# Search for apps matching "Clash Royale"
app_info <- get_unified_app_info(term = "Clash Royale", limit = 1)
print(app_info)
# Use the unified_app_id from app_info in other functions if needed

# --- Example 2: Get Publisher's Apps ---
# Fetch apps published by Supercell (replace with a valid ID)
publisher_apps <- get_publisher_games(publisher_id = "560c48b48ac350643900b82d")
print(publisher_apps)

# --- Example 3: Fetch Detailed Metrics for One App ---
# Fetch metrics for a specific app ID (replace with a valid ID)
# Requires a valid unified_app_id known beforehand
metrics <- fetch_sensor_tower_metrics(
  unified_app_id = "YOUR_VALID_UNIFIED_APP_ID",
  start_date = Sys.Date() - 15,
  end_date = Sys.Date() - 1
)
print(metrics)
head(metrics)

# --- Example 4: Get Top Apps by Downloads ---
# Get top 5 iOS Games by downloads ("units") in the US for the last full month
top_downloads <- get_top_apps_by_revenue_and_downloads(
  os = "ios",
  comparison_attribute = "absolute", # Or "delta", "transformed_delta"
  time_range = "month",
  measure = "units",
  date = floor_date(Sys.Date() - months(1), "month"), # Start of last month
  category = 6000, # iOS Games category ID
  regions = "US",  # Region is required
  limit = 5
  # device_type defaults to "total" for iOS
)
print(top_downloads)

# --- Example 5: Get Top Apps by Active Users ---
# Get top 3 Android apps by MAU worldwide for the last quarter
top_mau <- get_top_apps_by_active_users(
  os = "android",
  comparison_attribute = "absolute",
  time_range = "quarter",
  measure = "MAU",
  date = floor_date(Sys.Date() - months(3), "quarter"), # Start of last quarter
  regions = "WW", # Worldwide region is required
  # category = NULL, # Optional: omit for all categories
  limit = 3
)
print(top_mau)

```

## Example Workflow: Comparing Pokemon Game Launches

This workflow demonstrates fetching data for multiple apps, combining it, and plotting launch performance.

```r
# Load required packages
library(sensortowerR)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(forcats)
library(tibble) # Added for tibble() usage
library(tidyr)  # Added for pivot_longer
library(scales) # Added for scale_y_continuous labels

# --- 1. Find App IDs ---
# Note: Ensure SENSORTOWER_AUTH_TOKEN is set in environment
pokemon_terms <- c("Pokémon GO", "Pokémon UNITE", "Pokémon Masters EX") # Add more if needed

get_id <- function(term) {
  info <- get_unified_app_info(term = term, limit = 1)
  if (nrow(info) > 0) {
    # Return a tibble to handle cases where info might be empty
    return(tibble(unified_app_name = info$unified_app_name[[1]], unified_app_id = info$unified_app_id[[1]]))
  } else {
    warning("Could not find app info for term: ", term)
    return(tibble(unified_app_name = term, unified_app_id = NA_character_)) # Return NA if not found
  }
}

# Get IDs for the specified Pokemon games
top_pokemon_games <- map_dfr(pokemon_terms, get_id) %>%
  filter(!is.na(unified_app_id)) # Remove any not found

print("Found Pokemon Game IDs:")
print(top_pokemon_games)

# --- 2. Add Launch Dates (Manual Step) ---
# You might need to find these dates elsewhere
top_pokemon_games <- top_pokemon_games %>%
  mutate(
    launch_date = case_when(
      unified_app_name == "Pokémon GO"       ~ ymd("2016-07-06"),
      unified_app_name == "Pokémon UNITE"    ~ ymd("2021-09-22"),
      # unified_app_name == "Pokémon TCG Pocket" ~ ymd("2024-10-30"), # Example future date
      unified_app_name == "Pokémon Masters EX" ~ ymd("2019-08-28"), # Corrected launch date approx
      TRUE                                   ~ NA_Date_
    )
  ) %>%
  filter(!is.na(launch_date)) # Keep only those with launch dates

print("Games with Launch Dates:")
print(top_pokemon_games)


# --- 3. Fetch Metrics Around Launch ---
# Define how many days post-launch to fetch
days_to_fetch <- 15

# Function to fetch metrics for one app
fetch_launch_metrics <- function(unified_app_id, unified_app_name, launch_date) {
  message("Fetching metrics for: ", unified_app_name)
  metrics <- fetch_sensor_tower_metrics(
    unified_app_id = unified_app_id,
    start_date = launch_date,
    end_date = launch_date + days(days_to_fetch) # Fetch for N days
  )

  # Add app info back if metrics were returned
  if (nrow(metrics) > 0) {
     metrics %>%
        # Ensure unified_app_id is present (should be from fetch function)
        # Add name and launch date for context
        mutate(
          unified_app_name = unified_app_name,
          launch_date = launch_date
        )
  } else {
    # Return an empty tibble matching expected structure if fetch failed
    tibble(
        unified_app_id = character(), date = as.Date(character()), country = character(),
        revenue = numeric(), downloads = numeric(), active_users = numeric(),
        unified_app_name = character(), launch_date = as.Date(character())
    )
  }
}

# Use pmap_dfr to iterate over rows of top_pokemon_games
# Ensure columns passed match function arguments: unified_app_id, unified_app_name, launch_date
combined_metrics <- top_pokemon_games %>%
  select(unified_app_id, unified_app_name, launch_date) %>% # Select only needed columns in correct order
  pmap_dfr(fetch_launch_metrics)

print("Combined Metrics Sample:")
print(head(combined_metrics))

# --- 4. Process and Plot ---
# Check if combined_metrics has data before proceeding
if (nrow(combined_metrics) > 0) {

  plot_data <- combined_metrics %>%
    mutate(
      date = as.Date(date),
      days_since_launch = as.numeric(date - launch_date),
      country = factor(country)
    ) %>%
    filter(country %in% c("US", "JP")) %>% # Filter for specific countries
    # Group by app, country, and days since launch to sum metrics if needed (e.g., if API splits by device)
    # Note: fetch_sensor_tower_metrics should already provide unified daily totals
    group_by(unified_app_name, country, days_since_launch) %>%
    summarize(
      daily_downloads = sum(downloads, na.rm = TRUE), # Use 'downloads' column
      daily_revenue = sum(revenue, na.rm = TRUE),     # Use 'revenue' column
      .groups = "drop"
    ) %>%
    # Calculate cumulative totals
    group_by(unified_app_name, country) %>%
    arrange(days_since_launch) %>% # Ensure correct order for cumsum
    mutate(
      total_downloads = cumsum(daily_downloads),
      total_revenue = cumsum(daily_revenue)
    ) %>%
    ungroup() %>%
    # Filter for the specific day we want to compare
    filter(days_since_launch == days_to_fetch) %>%
    # Pivot for plotting
    tidyr::pivot_longer( # Use tidyr::pivot_longer explicitly
      cols = c(total_downloads, total_revenue),
      names_to = "metric",
      values_to = "value"
    )

  # Check if plot_data has rows before plotting
  if (nrow(plot_data) > 0) {
    # Generate the plot
    launch_plot <- ggplot(plot_data, aes(x = fct_reorder(unified_app_name, value), y = value, fill = metric)) +
      geom_col(position = "dodge") + # Use geom_col for pre-summarized data
      facet_grid(metric ~ country, scales = "free_y") + # Use free_y for different scales
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) + # Nicer labels
      labs(
        title = paste("Cumulative Metrics", days_to_fetch, "Days Post-Launch"),
        subtitle = "Comparing Downloads and Revenue in US & JP",
        x = "Game",
        y = "Cumulative Value",
        fill = "Metric"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )

    print(launch_plot)

  } else {
    message("No data available for plotting after filtering.")
  }

} else {
  message("No combined metrics data was fetched successfully.")
}

```

## Dependencies

Core package dependencies (managed via `DESCRIPTION`):

-   httr (`get_unified_app_info`, `get_publisher_games`)
-   httr2 (`get_top_apps_...` functions)
-   jsonlite
-   dplyr
-   tibble
-   rlang
-   utils (implicitly used, e.g., `str`, `head`)

Workflow dependencies (install separately if running the workflow):

-   purrr
-   lubridate
-   ggplot2
-   forcats
-   tidyr (for `pivot_longer`)
-   scales (for plot labels)

```