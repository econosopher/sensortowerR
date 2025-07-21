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

# FiveThirtyEight-inspired color palette
fte_colors <- list(
  background = "#F0F0F0",
  text_primary = "#222222",
  text_secondary = "#606060",
  accent_orange = "#FF6600",
  accent_blue = "#008FD5",
  gray_light = "#ECECEC",
  gray_medium = "#CCCCCC",
  gray_dark = "#777777",
  highlight = "#FFEC8B",
  # Heatmap colors - subtle gradients
  heatmap_low = "#FAFAFA",
  heatmap_mid = "#77C3EC",
  heatmap_high = "#008FD5"
)

# Use well-known Role Playing category ID
role_playing_id <- 7014  # Role Playing Games category

# Get top Role Playing games
top_rpgs <- st_top_charts(
  measure = "revenue",
  category = role_playing_id,
  limit = 15
)

# Prepare the data with rank - no gender data available, just age
top_games <- top_rpgs %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    # Demographics (to be moved near rank) - only age is available
    any_of(c("age_ww")),
    # App Identity
    app_name = unified_app_name,
    # Top revenue country (to be first in revenue section)
    any_of(c("entities.custom_tags.Most Popular Country by Revenue", "most_popular_country_revenue")),
    # Revenue metrics
    any_of(c("revenue_180d_ww", "revenue_30d_ww", "revenue_first30d_ww",
            "revenue_alltime_ww", "revenue_alltime_us")),
    # User metrics
    any_of(c("dau_30d_ww", "dau_30d_us", "wau_4w_ww", "wau_4w_us",
            "mau_month_ww", "mau_month_us")),
    # Downloads
    any_of(c("downloads_180d_ww", "downloads_30d_ww", "downloads_first30d_ww",
            "downloads_alltime_ww", "downloads_alltime_us")),
    # Monetization
    any_of(c("rpd_alltime_ww", "rpd_alltime_us")),
    # Retention
    any_of(c("retention_1d_ww", "retention_1d_us", "retention_7d_ww",
            "retention_7d_us", "retention_14d_ww", "retention_14d_us",
            "retention_30d_ww", "retention_30d_us", "retention_60d_ww",
            "retention_60d_us")),
    # Release dates
    any_of(c("release_date_us", "release_date_ww")),
    # Other demographics  
    any_of(c("age_us", "genders_us"))
  )

# Create the GT table with FiveThirtyEight styling
rpg_table_538 <- top_games %>%
  gt() %>%
  
  # Add title and subtitle in 538 style
  tab_header(
    title = "Top Squad RPG Games: Performance Metrics",
    subtitle = "Revenue, downloads, retention, and engagement data for leading titles"
  ) %>%
  
  # Rename columns for better readability
  cols_label(
    rank = "Rank",
    age_ww = "Avg Age",
    app_name = "Game",
    `entities.custom_tags.Most Popular Country by Revenue` = "Top Country",
    release_date_us = "US Release",
    release_date_ww = "WW Release",
    revenue_180d_ww = "180d Rev",
    revenue_30d_ww = "30d Rev",
    revenue_first30d_ww = "First 30d Rev",
    revenue_alltime_ww = "Lifetime Rev (WW)",
    revenue_alltime_us = "Lifetime Rev (US)",
    dau_30d_ww = "DAU (WW)",
    dau_30d_us = "DAU (US)",
    wau_4w_ww = "WAU (WW)",
    wau_4w_us = "WAU (US)",
    mau_month_ww = "MAU (WW)",
    mau_month_us = "MAU (US)",
    downloads_180d_ww = "180d DLs",
    downloads_30d_ww = "30d DLs",
    downloads_first30d_ww = "First 30d DLs",
    downloads_alltime_ww = "Lifetime DLs (WW)",
    downloads_alltime_us = "Lifetime DLs (US)",
    rpd_alltime_ww = "RPD (WW)",
    rpd_alltime_us = "RPD (US)",
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
    age_us = "Age (US)"
  ) %>%
  
  # Group columns with spanners
  tab_spanner(
    label = "Demographics",
    columns = any_of(c("age_ww"))
  ) %>%
  
  tab_spanner(
    label = "Revenue",
    columns = any_of(c("entities.custom_tags.Most Popular Country by Revenue", "most_popular_country_revenue", 
                      "revenue_180d_ww", "revenue_30d_ww", 
                      "revenue_first30d_ww", "revenue_alltime_ww", "revenue_alltime_us"))
  ) %>%
  
  tab_spanner(
    label = "Active Users",
    columns = any_of(c("dau_30d_ww", "dau_30d_us", "wau_4w_ww", "wau_4w_us",
                      "mau_month_ww", "mau_month_us"))
  ) %>%
  
  tab_spanner(
    label = "Downloads",
    columns = any_of(c("downloads_180d_ww", "downloads_30d_ww", "downloads_first30d_ww",
                      "downloads_alltime_ww", "downloads_alltime_us"))
  ) %>%
  
  tab_spanner(
    label = "Monetization",
    columns = any_of(c("rpd_alltime_ww", "rpd_alltime_us"))
  ) %>%
  
  tab_spanner(
    label = "Retention % (WW)",
    columns = any_of(c("retention_1d_ww", "retention_7d_ww", 
                      "retention_14d_ww", "retention_30d_ww", "retention_60d_ww"))
  ) %>%
  
  tab_spanner(
    label = "Retention % (US)",
    columns = any_of(c("retention_1d_us", "retention_7d_us", 
                      "retention_14d_us", "retention_30d_us", "retention_60d_us"))
  ) %>%
  
  tab_spanner(
    label = "Release Info",
    columns = any_of(c("release_date_us", "release_date_ww"))
  ) %>%

  # Format rank column
  fmt_number(
    columns = rank,
    decimals = 0
  ) %>%
  
  # Format numbers with separators and suffixes
  fmt_number(
    columns = any_of(c("dau_30d_ww", "dau_30d_us", "wau_4w_ww", "wau_4w_us",
                      "mau_month_ww", "mau_month_us",
                      "downloads_30d_ww", "downloads_180d_ww", "downloads_first30d_ww",
                      "downloads_alltime_us", "downloads_alltime_ww")),
    decimals = 0,
    use_seps = TRUE,
    suffixing = TRUE
  ) %>%

  # Format currency values with suffixes
  fmt_currency(
    columns = any_of(c("revenue_180d_ww", "revenue_30d_ww", "revenue_first30d_ww",
                      "revenue_alltime_ww", "revenue_alltime_us")),
    currency = "USD",
    decimals = 0,
    suffixing = TRUE
  ) %>%

  # Format RPD with 2 decimal places
  fmt_currency(
    columns = any_of(c("rpd_alltime_ww", "rpd_alltime_us")),
    currency = "USD",
    decimals = 2,
    suffixing = TRUE
  ) %>%

  # Format percentages
  fmt_percent(
    columns = any_of(c("retention_1d_ww", "retention_1d_us",
                      "retention_7d_ww", "retention_7d_us",
                      "retention_14d_ww", "retention_14d_us",
                      "retention_30d_ww", "retention_30d_us",
                      "retention_60d_ww", "retention_60d_us")),
    decimals = 0,
    scale = FALSE
  ) %>%

  # Format age
  fmt_number(
    columns = any_of(c("age_ww", "age_us")),
    decimals = 0
  ) %>%

  # Format missing values
  sub_missing(
    columns = everything(),
    missing_text = "—"
  ) %>%
  
  # Apply FiveThirtyEight-style retention heatmap
  # Use subtle blue gradient for retention metrics
  data_color(
    columns = any_of(c("retention_1d_ww", "retention_7d_ww", "retention_14d_ww",
                      "retention_30d_ww", "retention_60d_ww")),
    method = "numeric",
    palette = c(fte_colors$heatmap_low, fte_colors$heatmap_mid, fte_colors$heatmap_high),
    domain = c(0, 100),
    na_color = fte_colors$gray_light,
    apply_to = "fill",
    autocolor_text = FALSE
  ) %>%
  
  data_color(
    columns = any_of(c("retention_1d_us", "retention_7d_us", "retention_14d_us",
                      "retention_30d_us", "retention_60d_us")),
    method = "numeric",
    palette = c(fte_colors$heatmap_low, fte_colors$heatmap_mid, fte_colors$heatmap_high),
    domain = c(0, 100),
    na_color = fte_colors$gray_light,
    apply_to = "fill",
    autocolor_text = FALSE
  ) %>%
  
  # Apply global table options for 538 style
  tab_options(
    # Overall table styling
    table.background.color = "white",
    table.font.names = c("-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif"),
    table.font.size = px(12),
    table.font.color = fte_colors$text_primary,
    
    # Header styling
    heading.background.color = "white",
    heading.title.font.size = px(20),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(14),
    heading.subtitle.font.weight = "normal",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = fte_colors$gray_dark,
    heading.align = "left",
    
    # Column labels
    column_labels.background.color = fte_colors$gray_light,
    column_labels.font.weight = "bold",
    column_labels.font.size = px(11),
    column_labels.border.top.width = px(2),
    column_labels.border.top.color = fte_colors$gray_dark,
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = fte_colors$gray_medium,
    
    # Row styling
    row.striping.include_table_body = FALSE,
    table_body.hlines.color = fte_colors$gray_light,
    table_body.hlines.width = px(1),
    table_body.border.bottom.width = px(2),
    table_body.border.bottom.color = fte_colors$gray_dark,
    
    # Cell padding
    data_row.padding = px(5)
  ) %>%
  
  # Style the title
  tab_style(
    style = list(
      cell_text(
        font = c("-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif"),
        weight = "bold",
        color = fte_colors$text_primary
      )
    ),
    locations = cells_title(groups = "title")
  ) %>%
  
  # Style the subtitle
  tab_style(
    style = list(
      cell_text(
        font = c("-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif"),
        color = fte_colors$text_secondary
      )
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  # Style column spanners with minimal background
  tab_style(
    style = list(
      cell_fill(color = fte_colors$background),
      cell_text(
        weight = "bold",
        size = px(12),
        color = fte_colors$text_primary
      ),
      cell_borders(
        sides = "bottom",
        color = fte_colors$gray_medium,
        weight = px(1)
      )
    ),
    locations = cells_column_spanners()
  ) %>%
  
  # Style rank column
  tab_style(
    style = list(
      cell_fill(color = fte_colors$background),
      cell_text(
        weight = "bold",
        align = "center",
        color = fte_colors$text_primary
      )
    ),
    locations = cells_body(columns = rank)
  ) %>%
  
  # Center align rank header
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels(columns = rank)
  ) %>%
  
  # Highlight top 3 games with subtle yellow
  tab_style(
    style = list(
      cell_fill(color = fte_colors$highlight)
    ),
    locations = cells_body(
      rows = 1:3,
      columns = any_of(c("rank", "age_ww", "app_name"))
    )
  ) %>%
  
  # Add subtle borders between major sections
  tab_style(
    style = cell_borders(
      sides = "right",
      color = fte_colors$gray_medium,
      weight = px(1)
    ),
    locations = cells_body(
      columns = any_of(c("rank", "age_ww", "app_name", 
                        "entities.custom_tags.Most Popular Country by Revenue",
                        "revenue_alltime_us", "mau_month_us", "downloads_alltime_us", 
                        "rpd_alltime_us", "retention_60d_ww", "retention_60d_us", 
                        "release_date_ww"))
    )
  ) %>%
  
  # Make text in retention cells contrast better with background
  tab_style(
    style = list(
      cell_text(
        color = fte_colors$text_primary,
        weight = "normal"
      )
    ),
    locations = cells_body(
      columns = any_of(c("retention_1d_ww", "retention_1d_us",
                        "retention_7d_ww", "retention_7d_us",
                        "retention_14d_ww", "retention_14d_us",
                        "retention_30d_ww", "retention_30d_us",
                        "retention_60d_ww", "retention_60d_us"))
    )
  )

# Save as HTML
gtsave(rpg_table_538, "output/rpg_rankings_538style.html")

# Save as PNG
gtsave(rpg_table_538, "output/rpg_rankings_538style.png", vwidth = 1800, vheight = 900)

# Print message
print("FiveThirtyEight-styled RPG rankings table saved to output/rpg_rankings_538style.html and .png")