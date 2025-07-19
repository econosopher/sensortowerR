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


# Use well-known Role Playing category ID
role_playing_id <- 7014  # Role Playing Games category

# ---- Main Analysis: Get Top 20 Role Playing Games using NEW Unified Function ----
  top_rpgs <- st_top_charts(
    measure = "MAU",
    category = role_playing_id
    # Using defaults: os="unified", regions="WW", date=current month start, limit=20
  )

  # --- Create a Presentation Table with Enhanced Metrics ---
  table_data <- top_rpgs %>%
    select(
      # App Identity
      unified_app_name,

      # Monthly Active Users & Engagement
      mau_month_us,
      mau_month_ww,
      dau_30d_us,
      dau_30d_ww,

      # Downloads Performance
      downloads_30d_ww,
      downloads_180d_ww,
      downloads_alltime_ww,

      # Revenue Performance
      revenue_30d_ww,
      revenue_180d_ww,
      revenue_alltime_ww,

      # Key Performance Indicators
      rpd_alltime_us,
      rpd_alltime_ww,
      retention_1d_us,
      retention_7d_us,
      retention_30d_us,

      # Demographics & Launch
      age_us,
      release_date_us
    ) %>%
    # Convert character data to numeric for proper GT formatting
    arrange(desc(mau_month_ww))  # Sort by worldwide MAU

# --- Create Enhanced Presentation Table ---
  # Select and arrange columns logically by category
  table_data <- top_rpgs %>%
    select(
      # App Identity & Demographics (moved up front)
      unified_app_name,
      age_us,
      release_date_us,

      # Monthly Active Users & Engagement (Worldwide first)
      mau_month_ww,
      mau_month_us,
      dau_30d_ww,
      dau_30d_us,

      # Downloads Performance
      downloads_30d_ww,
      downloads_180d_ww,
      downloads_alltime_ww,

      # Revenue Performance
      revenue_30d_ww,
      revenue_180d_ww,
      revenue_alltime_ww,

      # Key Performance Indicators (Worldwide first)
      rpd_alltime_ww,
      rpd_alltime_us,
      retention_1d_us,
      retention_7d_us,
      retention_30d_us
    ) %>%
    arrange(desc(mau_month_ww))  # Sort by worldwide MAU

  # Create the enhanced gt table
  gt_table <- table_data %>%
    gt(rowname_col = "unified_app_name") %>%

    # Header
    tab_header(
      title = "Top Role Playing Games Analytics",
      subtitle = paste("Performance Dashboard -", format(Sys.Date(), "%B %Y"))
    ) %>%

    # Organize columns into logical groups (Worldwide metrics first)
    tab_spanner(
      label = "App Info",
      columns = c(age_us, release_date_us)
    ) %>%
    tab_spanner(
      label = "Monthly Active Users",
      columns = c(mau_month_ww, mau_month_us)
    ) %>%
    tab_spanner(
      label = "Daily Active Users (30d)",
      columns = c(dau_30d_ww, dau_30d_us)
    ) %>%
    tab_spanner(
      label = "Downloads Performance",
      columns = c(downloads_30d_ww, downloads_180d_ww, downloads_alltime_ww)
    ) %>%
    tab_spanner(
      label = "Revenue Performance",
      columns = c(revenue_30d_ww, revenue_180d_ww, revenue_alltime_ww)
    ) %>%
    tab_spanner(
      label = "Revenue Per Download",
      columns = c(rpd_alltime_ww, rpd_alltime_us)
    ) %>%
    tab_spanner(
      label = "User Retention",
      columns = c(retention_1d_us, retention_7d_us, retention_30d_us)
    ) %>%

    # Clean column labels
    cols_label(
      age_us = "Avg Age",
      release_date_us = "Launch Date",
      mau_month_ww = "Worldwide",
      mau_month_us = "US",
      dau_30d_ww = "Worldwide",
      dau_30d_us = "US",
      downloads_30d_ww = "30 Days",
      downloads_180d_ww = "180 Days",
      downloads_alltime_ww = "All Time",
      revenue_30d_ww = "30 Days",
      revenue_180d_ww = "180 Days",
      revenue_alltime_ww = "All Time",
      rpd_alltime_ww = "Worldwide",
      rpd_alltime_us = "US",
      retention_1d_us = "Day 1",
      retention_7d_us = "Day 7",
      retention_30d_us = "Day 30"
    ) %>%

    # Format numbers with separators and suffixes
    fmt_number(
      columns = c(mau_month_us, mau_month_ww, dau_30d_us, dau_30d_ww,
                  downloads_30d_ww, downloads_180d_ww, downloads_alltime_ww),
      decimals = 0,
      use_seps = TRUE,
      suffixing = TRUE
    ) %>%

    # Format currency values with suffixes
    fmt_currency(
      columns = c(revenue_30d_ww, revenue_180d_ww, revenue_alltime_ww),
      currency = "USD",
      decimals = 0,
      suffixing = TRUE
    ) %>%

    # Format RPD with 2 decimal places
    fmt_currency(
      columns = c(rpd_alltime_ww, rpd_alltime_us),
      currency = "USD",
      decimals = 2,
      suffixing = TRUE
    ) %>%

    # Format percentages
    fmt_percent(
      columns = c(retention_1d_us, retention_7d_us, retention_30d_us),
      decimals = 1
    ) %>%

    # Format age
    fmt_number(
      columns = age_us,
      decimals = 0
    ) %>%


    # Add data bars for top metrics
    tab_style(
      style = cell_borders(
        sides = "right",
        color = "#E5E5E5",
        weight = px(1)
      ),
      locations = cells_body(
        columns = c(age_us, mau_month_us, dau_30d_us, downloads_alltime_ww,
                   revenue_alltime_ww, rpd_alltime_us, retention_30d_us)
      )
    ) %>%

    # Add source note
    tab_source_note(
      source_note = "Source: Sensor Tower API"
    )


print(gt_table)
