# Load required packages
# We use devtools to load the local, in-development version of the package.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  char = c(
    "devtools",
    "gt",
    "dplyr",
    "lubridate",
    "tidyr"
  )
)

# Load the development version of sensortowerR from the project root
devtools::load_all()

# ---- Investigation: Find Category ID for "Role Playing" ----

# The goal is to find the `category_id` for "Role Playing" games.
# We can do this by finding a known Squad RPG, like "Marvel Strike Force",
# and inspecting its enriched category data.
# We use st_app_info with `return_all_fields = TRUE` and `limit = 1`.

message("Investigating 'Marvel Strike Force' to find the Role Playing category ID...")

# Use the package function to get detailed app info for the top search result
marvel_strike_force_info <- st_app_info(
  term = "Marvel Strike Force",
  return_all_fields = TRUE,
  limit = 1
)

# The `category_details` column is a list containing a tibble of categories.
# We unnest it to work with the category information.
if (!is.null(marvel_strike_force_info$category_details)) {
  category_df <- marvel_strike_force_info %>%
    select("name", "category_details") %>%
    tidyr::unnest(cols = c(category_details))

  # Find the ID for "Role Playing" (can be "Games/Role Playing" on iOS)
  # We search for any category name containing "Role Playing" to be flexible.
  role_playing_id <- category_df %>%
    filter(grepl("Role Playing", category_name)) %>%
    pull(category_id) %>%
    first() # Take the first match

} else {
  role_playing_id <- NULL
  warning("Could not find the 'category_details' column in the API response.")
}

if (!is.null(role_playing_id) && !is.na(role_playing_id)) {
  message(paste("Successfully found Role Playing Category ID:", role_playing_id))
} else {
  warning("Could not find a Category ID for 'Role Playing'. Halting analysis.")
  role_playing_id <- NULL # Set to NULL to stop execution
}

# ---- Main Analysis: Get Top 20 Role Playing Games ----

# Proceed only if we found a category ID
if (!is.null(role_playing_id)) {
  message("Fetching the top 20 Role Playing games by MAU...")
  top_rpgs <- st_top_active_users(
    comparison_attribute = "absolute",
    time_range = "month", 
    measure = "MAU",
    category = role_playing_id
    # Using defaults: os="unified", regions="WW", date=current month start, limit=20
  )

  # --- 2. Create a Presentation Table with gt ---
  if (nrow(top_rpgs) > 0) {
    # Select and arrange columns for the table using enhanced metrics
    table_data <- top_rpgs %>%
      select(
        unified_app_name,
        entities.users_absolute,
        entities.users_delta,
        entities.users_transformed_delta,
        # Enhanced custom metrics with clean names
        downloads_180d_ww,
        revenue_180d_ww,
        retention_1d_us,
        rpd_alltime_us
      ) %>%
      filter(!is.na(unified_app_name)) %>%  # Remove rows without app names
      arrange(desc(entities.users_absolute))

          # Create the gt table with enhanced metrics
      gt_table <- table_data %>%
        gt(rowname_col = "unified_app_name") %>%
        tab_header(
          title = "Top 20 Role Playing Games - Enhanced Analytics",
          subtitle = paste("Worldwide -", format(lubridate::floor_date(Sys.Date(), "month"), "%B %Y"))
        ) %>%
        cols_label(
          entities.users_absolute = "Current MAU",
          entities.users_delta = "MAU Change",
          entities.users_transformed_delta = "Growth %",
          downloads_180d_ww = "Downloads (180d)",
          revenue_180d_ww = "Revenue (180d)", 
          retention_1d_us = "Day 1 Retention",
          rpd_alltime_us = "RPD (All Time)"
        ) %>%
        fmt_number(
          columns = c(entities.users_absolute, entities.users_delta, downloads_180d_ww),
          decimals = 0,
          use_seps = TRUE
        ) %>%
        fmt_currency(
          columns = c(revenue_180d_ww, rpd_alltime_us),
          currency = "USD",
          decimals = 0
        ) %>%
        fmt_percent(
          columns = c(entities.users_transformed_delta, retention_1d_us),
          decimals = 1
        ) %>%
        data_color(
            columns = entities.users_transformed_delta,
            colors = scales::col_numeric(
                palette = c("red", "white", "green"),
                domain = c(-1, 1)
            )
        ) %>%
        tab_source_note(
          source_note = "Source: Sensor Tower API | Enhanced with custom metrics extraction"
        )

      # Print the table
      print(gt_table)

  } else {
    message("No data returned from API for the given category ID.")
  }
} else {
  message("No valid Category ID found. Please check the investigation steps.")
}
