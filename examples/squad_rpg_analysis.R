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
    select(.data$name, .data$category_details) %>%
    tidyr::unnest(cols = c(category_details))

  # Find the ID for "Role Playing" (can be "Games/Role Playing" on iOS)
  # We search for any category name containing "Role Playing" to be flexible.
  role_playing_id <- category_df %>%
    filter(grepl("Role Playing", .data$category_name)) %>%
    pull(.data$category_id) %>%
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
  message("Fetching the top 20 Role Playing games by revenue...")
  top_rpgs <- st_top_active_users(
    os = "unified",
    comparison_attribute = "absolute",
    time_range = "month",
    measure = "MAU",
    date = floor_date(Sys.Date() - months(1), "month"),
    regions = "WW",
    limit = 20,
    category = role_playing_id
  )

  # --- 2. Create a Presentation Table with gt ---
  if (nrow(top_rpgs) > 0) {
    # Select and arrange columns for the table
    table_data <- top_rpgs %>%
      select(
        app.name,
        users_absolute,
        users_delta,
        users_transformed_delta
      ) %>%
      arrange(desc(.data$users_absolute))

    # Create the gt table
    gt_table <- table_data %>%
      gt(rowname_col = "app.name") %>%
      tab_header(
        title = "Top 20 Role Playing Games by Monthly Active Users",
        subtitle = paste("Worldwide -", format(floor_date(Sys.Date() - months(1), "month"), "%B %Y"))
      ) %>%
      cols_label(
        users_absolute = "MAU",
        users_delta = "Change",
        users_transformed_delta = "Growth"
      ) %>%
      fmt_number(
        columns = c(users_absolute, users_delta),
        decimals = 0,
        use_seps = TRUE
      ) %>%
      fmt_percent(
        columns = users_transformed_delta,
        decimals = 1
      ) %>%
      data_color(
          columns = users_transformed_delta,
          colors = scales::col_numeric(
              palette = c("red", "white", "green"),
              domain = c(-1, 1)
          )
      ) %>%
      tab_source_note(
        source_note = "Source: Sensor Tower API"
      )

      # Print the table
      print(gt_table)

  } else {
    message("No data returned from API for the given category ID.")
  }
} else {
  message("No valid Category ID found. Please check the investigation steps.")
}
