# Modern Squad RPG Analysis Dashboard
# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  char = c(
    "devtools",
    "gt",
    "dplyr",
    "lubridate",
    "tidyr",
    "ggplot2",
    "scales",
    "stringr",
    "webshot2"
  )
)

# Load the development version of sensortowerR
devtools::load_all()

# Modern color palette inspired by contemporary design systems
modern_colors <- list(
  # Primary colors
  primary = "#1a73e8",      # Google Blue
  primary_light = "#e8f0fe",
  primary_dark = "#1967d2",
  
  # Neutrals
  text_primary = "#202124",
  text_secondary = "#5f6368",
  text_tertiary = "#80868b",
  background = "#ffffff",
  surface = "#f8f9fa",
  border = "#dadce0",
  border_light = "#e8eaed",
  
  # Status colors
  success = "#1e8e3e",
  success_light = "#e6f4ea",
  warning = "#f9ab00",
  warning_light = "#fef7e0",
  error = "#d33b3b",
  error_light = "#fce8e6",
  
  # Data visualization
  chart_blue = "#4285f4",
  chart_green = "#0f9d58",
  chart_yellow = "#f4b400",
  chart_red = "#ea4335",
  
  # Heatmap gradient (subtle blue)
  heat_low = "#f8f9fa",
  heat_mid = "#c7e0f4",
  heat_high = "#1a73e8"
)

# Use well-known Role Playing category ID
role_playing_id <- 7014

# Get top Role Playing games
top_rpgs <- st_top_charts(
  measure = "revenue",
  category = role_playing_id,
  limit = 20
)

# Prepare data with proper column selection
table_data <- top_rpgs %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    app_name = unified_app_name,
    
    # Revenue metrics
    revenue_180d_ww,
    any_of(c("revenue_30d_ww", "revenue_first30d_ww", 
            "revenue_alltime_ww", "revenue_alltime_us")),
    
    # RPD
    any_of(c("rpd_alltime_ww", "rpd_alltime_us")),
    
    # Downloads
    any_of(c("downloads_30d_ww", "downloads_180d_ww", "downloads_first30d_ww",
            "downloads_alltime_us", "downloads_alltime_ww")),
    
    # Active users
    any_of(c("dau_30d_ww", "dau_30d_us", "wau_4w_ww", "wau_4w_us",
            "mau_month_ww", "mau_month_us")),
    
    # Retention
    any_of(c("retention_1d_ww", "retention_1d_us", "retention_7d_ww",
            "retention_7d_us", "retention_14d_ww", "retention_14d_us",
            "retention_30d_ww", "retention_30d_us", "retention_60d_ww",
            "retention_60d_us")),
    
    # Demographics
    any_of(c("age_ww", "age_us")),
    
    # Release info
    any_of(c("release_date_us", "release_date_ww"))
  )

# Create modern GT table
modern_gt_table <- table_data %>%
  gt() %>%
  
  # Header with modern typography
  tab_header(
    title = "Squad RPG Performance Dashboard",
    subtitle = "Real-time analytics for top role-playing games"
  ) %>%
  
  # Column labels
  cols_label(
    rank = "",
    app_name = "Game",
    revenue_180d_ww = "180d",
    revenue_30d_ww = "30d",
    revenue_first30d_ww = "First 30d",
    revenue_alltime_ww = "Lifetime",
    revenue_alltime_us = "US Lifetime",
    rpd_alltime_ww = "Global",
    rpd_alltime_us = "US",
    downloads_30d_ww = "30d",
    downloads_180d_ww = "180d",
    downloads_first30d_ww = "First 30d",
    downloads_alltime_us = "US Lifetime",
    downloads_alltime_ww = "Lifetime",
    dau_30d_ww = "DAU",
    dau_30d_us = "DAU (US)",
    wau_4w_ww = "WAU",
    wau_4w_us = "WAU (US)",
    mau_month_ww = "MAU",
    mau_month_us = "MAU (US)",
    retention_1d_ww = "D1",
    retention_1d_us = "D1",
    retention_7d_ww = "D7",
    retention_7d_us = "D7",
    retention_14d_ww = "D14",
    retention_14d_us = "D14",
    retention_30d_ww = "D30",
    retention_30d_us = "D30",
    retention_60d_ww = "D60",
    retention_60d_us = "D60",
    age_ww = "Age",
    age_us = "Age (US)",
    release_date_us = "US Launch",
    release_date_ww = "Global Launch"
  ) %>%
  
  # Column groups
  tab_spanner(
    label = "Revenue ($)",
    columns = any_of(c("revenue_180d_ww", "revenue_30d_ww", "revenue_first30d_ww",
                      "revenue_alltime_ww", "revenue_alltime_us"))
  ) %>%
  
  tab_spanner(
    label = "RPD",
    columns = any_of(c("rpd_alltime_ww", "rpd_alltime_us"))
  ) %>%
  
  tab_spanner(
    label = "Downloads",
    columns = any_of(c("downloads_30d_ww", "downloads_180d_ww", "downloads_first30d_ww",
                      "downloads_alltime_us", "downloads_alltime_ww"))
  ) %>%
  
  tab_spanner(
    label = "Active Users",
    columns = any_of(c("dau_30d_ww", "dau_30d_us", "wau_4w_ww", "wau_4w_us",
                      "mau_month_ww", "mau_month_us"))
  ) %>%
  
  tab_spanner(
    label = "Retention % (Global)",
    columns = any_of(c("retention_1d_ww", "retention_7d_ww", 
                      "retention_14d_ww", "retention_30d_ww", "retention_60d_ww"))
  ) %>%
  
  tab_spanner(
    label = "Retention % (US)",
    columns = any_of(c("retention_1d_us", "retention_7d_us", 
                      "retention_14d_us", "retention_30d_us", "retention_60d_us"))
  ) %>%
  
  tab_spanner(
    label = "Demographics",
    columns = any_of(c("age_ww", "age_us"))
  ) %>%
  
  tab_spanner(
    label = "Release Dates",
    columns = any_of(c("release_date_us", "release_date_ww"))
  ) %>%
  
  # Number formatting
  fmt_number(
    columns = rank,
    decimals = 0
  ) %>%
  
  fmt_number(
    columns = any_of(c("dau_30d_ww", "dau_30d_us", "wau_4w_ww", "wau_4w_us",
                      "mau_month_ww", "mau_month_us",
                      "downloads_30d_ww", "downloads_180d_ww", "downloads_first30d_ww",
                      "downloads_alltime_us", "downloads_alltime_ww")),
    decimals = 0,
    suffixing = TRUE
  ) %>%
  
  fmt_currency(
    columns = any_of(c("revenue_180d_ww", "revenue_30d_ww", "revenue_first30d_ww",
                      "revenue_alltime_ww", "revenue_alltime_us")),
    currency = "USD",
    decimals = 0,
    suffixing = TRUE
  ) %>%
  
  fmt_currency(
    columns = any_of(c("rpd_alltime_ww", "rpd_alltime_us")),
    currency = "USD",
    decimals = 2
  ) %>%
  
  fmt_percent(
    columns = any_of(c("retention_1d_ww", "retention_1d_us",
                      "retention_7d_ww", "retention_7d_us",
                      "retention_14d_ww", "retention_14d_us",
                      "retention_30d_ww", "retention_30d_us",
                      "retention_60d_ww", "retention_60d_us")),
    decimals = 1,
    scale = FALSE
  ) %>%
  
  fmt_number(
    columns = any_of(c("age_ww", "age_us")),
    decimals = 0
  ) %>%
  
  sub_missing(
    columns = everything(),
    missing_text = "—"
  ) %>%
  
  # Modern styling with subtle heatmap for retention
  data_color(
    columns = any_of(c("retention_1d_ww", "retention_7d_ww", "retention_14d_ww",
                      "retention_30d_ww", "retention_60d_ww")),
    method = "numeric",
    palette = c(modern_colors$heat_low, modern_colors$heat_mid, modern_colors$heat_high),
    domain = c(0, 100),
    apply_to = "fill",
    autocolor_text = FALSE
  ) %>%
  
  data_color(
    columns = any_of(c("retention_1d_us", "retention_7d_us", "retention_14d_us",
                      "retention_30d_us", "retention_60d_us")),
    method = "numeric",
    palette = c(modern_colors$heat_low, modern_colors$heat_mid, modern_colors$heat_high),
    domain = c(0, 100),
    apply_to = "fill",
    autocolor_text = FALSE
  ) %>%
  
  # Apply modern table options
  tab_options(
    # Overall table
    table.background.color = modern_colors$background,
    table.font.names = c("-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Helvetica Neue", "Arial", "sans-serif"),
    table.font.size = px(13),
    table.font.color = modern_colors$text_primary,
    table.border.top.style = "none",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(1),
    table.border.bottom.color = modern_colors$border,
    
    # Header
    heading.background.color = modern_colors$background,
    heading.title.font.size = px(24),
    heading.title.font.weight = "600",
    heading.subtitle.font.size = px(14),
    heading.subtitle.font.weight = "400",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(1),
    heading.border.bottom.color = modern_colors$border_light,
    heading.align = "left",
    heading.padding = px(20),
    
    # Column labels
    column_labels.background.color = modern_colors$surface,
    column_labels.font.weight = "500",
    column_labels.font.size = px(12),
    column_labels.text_transform = "uppercase",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = modern_colors$border_light,
    column_labels.padding = px(12),
    
    # Spanners
    table_body.hlines.style = "solid",
    table_body.hlines.width = px(1),
    table_body.hlines.color = modern_colors$border_light,
    table_body.border.bottom.style = "solid",
    table_body.border.bottom.width = px(1),
    table_body.border.bottom.color = modern_colors$border,
    
    # Data rows
    data_row.padding = px(10),
    row.striping.include_table_body = FALSE,
    
    # Source notes
    source_notes.font.size = px(11),
    source_notes.padding = px(10)
  ) %>%
  
  # Style subtitle separately
  tab_style(
    style = list(
      cell_text(color = modern_colors$text_secondary)
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  # Style column labels text color
  tab_style(
    style = list(
      cell_text(
        color = modern_colors$text_secondary,
        transform = "uppercase",
        size = px(11)
      )
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Style the rank column
  tab_style(
    style = list(
      cell_fill(color = modern_colors$surface),
      cell_text(
        size = px(16),
        weight = "600",
        color = modern_colors$text_secondary,
        align = "center"
      )
    ),
    locations = cells_body(columns = rank)
  ) %>%
  
  # Style rank header
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = rank)
  ) %>%
  
  # Style column spanners
  tab_style(
    style = list(
      cell_text(
        weight = "500",
        size = px(11),
        color = modern_colors$text_primary
      ),
      cell_borders(
        sides = "bottom",
        color = modern_colors$border_light,
        weight = px(1)
      )
    ),
    locations = cells_column_spanners()
  ) %>%
  
  # Highlight top 3 with subtle accent
  tab_style(
    style = list(
      cell_fill(color = modern_colors$primary_light),
      cell_borders(
        sides = "left",
        color = modern_colors$primary,
        weight = px(3)
      )
    ),
    locations = cells_body(
      rows = 1:3,
      columns = everything()
    )
  ) %>%
  
  # Game name styling
  tab_style(
    style = list(
      cell_text(
        weight = "500",
        size = px(14)
      )
    ),
    locations = cells_body(columns = app_name)
  ) %>%
  
  # Add subtle vertical separators
  tab_style(
    style = cell_borders(
      sides = "right",
      color = modern_colors$border_light,
      weight = px(1)
    ),
    locations = cells_body(
      columns = any_of(c("rank", "app_name", "revenue_alltime_us", 
                        "rpd_alltime_us", "downloads_alltime_ww", 
                        "mau_month_us", "retention_60d_ww", 
                        "retention_60d_us", "age_us"))
    )
  ) %>%
  
  # Add source note
  tab_source_note(
    source_note = "Data: Sensor Tower API | Updated: July 2025"
  )

# Create output directory if it doesn't exist
output_dir <- "inst/images"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the modern table, overwriting the existing dashboard
modern_gt_table %>%
  gtsave(
    filename = file.path(output_dir, "rpg_analytics_dashboard.png"),
    vwidth = 1600,
    vheight = 1000
  )

cat("\nModern dashboard created successfully!\n")
cat("File saved to:", file.path(output_dir, "rpg_analytics_dashboard.png"), "\n")