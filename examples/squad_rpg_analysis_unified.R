# Squad RPG Analysis using NEW Unified st_top_charts Function
# Load required packages
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

# Load the development version of sensortowerR
devtools::load_all()

# ---- Use Known Role Playing Category ID ----
message("Using known Role Playing category ID for analysis...")

# Use well-known Role Playing category ID
role_playing_id <- 7014  # Role Playing Games category

message(paste("Using Role Playing Category ID:", role_playing_id))

# ---- Main Analysis: Get Top 20 Role Playing Games using NEW Unified Function ----
if (!is.null(role_playing_id)) {
  message("Fetching the top 20 Role Playing games by MAU using st_top_charts()...")
  
  # ⭐ Using the NEW unified function!
  top_rpgs <- st_top_charts(
    measure = "MAU",
    category = role_playing_id
    # Using defaults: os="unified", regions="WW", date=current month start, limit=20
  )

  # --- Create a Presentation Table with Enhanced Metrics ---
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
      filter(!is.na(unified_app_name)) %>%
      arrange(desc(entities.users_absolute))

    # Create the gt table with enhanced metrics
    gt_table <- table_data %>%
      gt(rowname_col = "unified_app_name") %>%
      tab_header(
        title = "Top 20 Role Playing Games - Enhanced Analytics (Unified Function)",
        subtitle = paste("Powered by st_top_charts() - Worldwide -", format(lubridate::floor_date(Sys.Date(), "month"), "%B %Y"))
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
        source_note = "Source: Sensor Tower API | Enhanced with st_top_charts() unified function"
      )

    # Print the table
    print(gt_table)

    # Show available custom metrics
    custom_metrics <- top_rpgs %>%
      select(matches("downloads_|revenue_|retention_|rpd_|arpu_")) %>%
      names()
    
    message("\n📊 Available enhanced metrics (", length(custom_metrics), " total):")
    for (metric in custom_metrics[1:8]) {  # Show first 8
      message("  ✓ ", metric)
    }
    message("  ... and ", length(custom_metrics) - 8, " more!")

  } else {
    message("No data returned from API for the given category ID.")
  }
} else {
  message("No valid Category ID found. Please check the investigation steps.")
} 