# Squad RPG Analysis using NEW Unified st_top_charts Function
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


# Use well-known Role Playing category ID
role_playing_id <- 7014  # Role Playing Games category

# ---- Main Analysis: Get Top 20 Role Playing Games using NEW Unified Function ----
  top_rpgs <- st_top_charts(
    measure = "revenue",
    category = role_playing_id
    # Using defaults: os="unified", regions="WW", date=current month start, limit=20
  )

  # --- Create a Presentation Table with Enhanced Metrics ---
  # Use dplyr::any_of() to handle missing columns gracefully
  # The data is already sorted by the measure used in st_top_charts (revenue in this case)
  # so we just add the rank based on the current order
  table_data <- top_rpgs %>%
    mutate(rank = row_number()) %>%  # Add rank column based on current sort order
    select(
      # App Identity and Rank
      unified_app_name,
      rank,
      
      # Revenue Performance (primary sort)
      revenue_180d_ww,  # This is the main revenue column
      any_of(c(
        "revenue_30d_ww",
        "revenue_first30d_ww",
        "most_popular_country_revenue",
        "revenue_alltime_ww",
        "revenue_alltime_us",
        "rpd_alltime_ww",
        "rpd_alltime_us"
      )),
      
      # Downloads Performance  
      any_of(c(
        "downloads_30d_ww",
        "downloads_180d_ww",
        "downloads_first30d_ww",
        "downloads_alltime_us",
        "downloads_alltime_ww"
      )),
      
      # Active Users (DAU, WAU, MAU)
      any_of(c(
        "dau_30d_ww",
        "dau_30d_us",
        "wau_4w_ww",
        "wau_4w_us",
        "mau_month_ww",
        "mau_month_us"
      )),
      
      # Retention Metrics
      any_of(c(
        "retention_1d_ww",
        "retention_1d_us",
        "retention_7d_ww",
        "retention_7d_us",
        "retention_14d_ww",
        "retention_14d_us",
        "retention_30d_ww",
        "retention_30d_us",
        "retention_60d_ww",
        "retention_60d_us"
      )),
      
      # Demographics
      any_of(c(
        "age_ww",
        "age_us",
        "genders_ww",
        "genders_us"
      )),
      
      # Release Dates
      any_of(c(
        "release_date_us",
        "release_date_ww"
      ))
    )


  # Define label mappings
  label_mappings <- c(
    rank = "Rank",
    revenue_180d_ww = "Revenue ($)",
    revenue_30d_ww = "Last 30 Days Revenue (WW)",
    revenue_first30d_ww = "Revenue First 30 Days (WW)",
    most_popular_country_revenue = "Most Popular Country by Revenue",
    revenue_alltime_ww = "All Time Revenue (WW)",
    revenue_alltime_us = "All Time Revenue (US)",
    rpd_alltime_ww = "RPD (All Time, WW)",
    rpd_alltime_us = "RPD (All Time, US)",
    downloads_30d_ww = "Last 30 Days Downloads (WW)",
    downloads_180d_ww = "Last 180 Days Downloads (WW)",
    downloads_first30d_ww = "Downloads First 30 Days (WW)",
    downloads_alltime_us = "All Time Downloads (US)",
    downloads_alltime_ww = "All Time Downloads (WW)",
    dau_30d_ww = "Last 30 Days Average DAU (WW)",
    dau_30d_us = "Last 30 Days Average DAU (US)",
    wau_4w_ww = "Last 4 Weeks Average WAU (WW)",
    wau_4w_us = "Last 4 Weeks Average WAU (US)",
    mau_month_ww = "Last Month Average MAU (WW)",
    mau_month_us = "Last Month Average MAU (US)",
    retention_1d_ww = "Day 1 Retention (Last Quarter, WW)",
    retention_1d_us = "Day 1 Retention (Last Quarter, US)",
    retention_7d_ww = "Day 7 Retention (Last Quarter, WW)",
    retention_7d_us = "Day 7 Retention (Last Quarter, US)",
    retention_14d_ww = "Day 14 Retention (Last Quarter, WW)",
    retention_14d_us = "Day 14 Retention (Last Quarter, US)",
    retention_30d_ww = "Day 30 Retention (Last Quarter, WW)",
    retention_30d_us = "Day 30 Retention (Last Quarter, US)",
    retention_60d_ww = "Day 60 Retention (Last Quarter, WW)",
    retention_60d_us = "Day 60 Retention (Last Quarter, US)",
    age_ww = "Age (Last Quarter, WW)",
    age_us = "Age (Last Quarter, US)",
    genders_ww = "Genders (Last Quarter, WW)",
    genders_us = "Genders (Last Quarter, US)",
    release_date_us = "Release Date (US)",
    release_date_ww = "Release Date (WW)"
  )
  
  # Only keep labels for columns that exist
  existing_cols <- intersect(names(label_mappings), names(table_data))
  labels_to_apply <- label_mappings[existing_cols]
  
  # Create the enhanced gt table
  gt_table <- table_data %>%
    gt(rowname_col = "unified_app_name") %>%

    # Header
    tab_header(
      title = "Top Role Playing Games Analytics",
      subtitle = paste("Performance Dashboard -", format(Sys.Date(), "%B %Y"))
    ) %>%

    # Apply column labels dynamically
    cols_label(!!!labels_to_apply) %>%
    
    # Add column spanners for logical groupings
    tab_spanner(
      label = "Revenue",
      columns = any_of(c("revenue_180d_ww", "revenue_30d_ww", "revenue_first30d_ww",
                         "most_popular_country_revenue", "revenue_alltime_ww", 
                         "revenue_alltime_us"))
    ) %>%
    
    tab_spanner(
      label = "Revenue Per Download (RPD)",
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
      label = "Retention Rates",
      columns = any_of(c("retention_1d_ww", "retention_1d_us", "retention_7d_ww",
                         "retention_7d_us", "retention_14d_ww", "retention_14d_us",
                         "retention_30d_ww", "retention_30d_us", "retention_60d_ww",
                         "retention_60d_us"))
    ) %>%
    
    tab_spanner(
      label = "Demographics",
      columns = any_of(c("age_ww", "age_us", "genders_ww", "genders_us"))
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
      decimals = 1,
      scale = FALSE  # Values are already in percentage format (e.g., 45 not 0.45)
    ) %>%

    # Format age
    fmt_number(
      columns = any_of(c("age_ww", "age_us")),
      decimals = 0
    ) %>%


    # Add styling for missing values
    fmt_missing(
      columns = everything(),
      missing_text = "—"
    ) %>%
    
    
    # Add gradient bars effect using data_color with custom settings
    # Revenue columns - create bar-like effect with gradient
    data_color(
      columns = any_of(c("revenue_180d_ww")),
      method = "numeric",
      palette = c("white", "#FF6B35"),
      domain = NULL,
      apply_to = "fill",
      autocolor_text = FALSE
    ) %>%
    
    data_color(
      columns = any_of(c("revenue_30d_ww", "revenue_alltime_ww")),
      method = "numeric",
      palette = c("white", "#FF8C42"),
      domain = NULL,
      apply_to = "fill",
      autocolor_text = FALSE
    ) %>%
    
    # Downloads columns - blue gradient bars
    data_color(
      columns = any_of(c("downloads_180d_ww")),
      method = "numeric",
      palette = c("white", "#1E88E5"),
      domain = NULL,
      apply_to = "fill",
      autocolor_text = FALSE
    ) %>%
    
    data_color(
      columns = any_of(c("downloads_30d_ww", "downloads_alltime_ww")),
      method = "numeric",
      palette = c("white", "#42A5F5"),
      domain = NULL,
      apply_to = "fill",
      autocolor_text = FALSE
    ) %>%
    
    # Active users - purple gradient
    data_color(
      columns = any_of(c("mau_month_ww", "dau_30d_ww")),
      method = "numeric",
      palette = c("white", "#9C27B0"),
      domain = NULL,
      apply_to = "fill",
      autocolor_text = FALSE
    ) %>%
    
    # Retention rates - red to green scale
    data_color(
      columns = any_of(c("retention_1d_us", "retention_7d_us", "retention_30d_us",
                         "retention_1d_ww", "retention_7d_ww", "retention_30d_ww")),
      method = "numeric",
      palette = c("#FFCDD2", "#4CAF50"),
      domain = c(0, 100),
      na_color = "#F5F5F5"
    ) %>%
    
    # Style the header
    tab_style(
      style = list(
        cell_text(weight = "bold", size = px(16))
      ),
      locations = cells_title(groups = "title")
    ) %>%
    
    # Style the column spanners
    tab_style(
      style = list(
        cell_fill(color = "#E8EAF6"),
        cell_text(weight = "bold", size = px(12))
      ),
      locations = cells_column_spanners()
    ) %>%
    
    # Style the column labels
    tab_style(
      style = list(
        cell_text(weight = "bold", size = px(11))
      ),
      locations = cells_column_labels()
    ) %>%
    
    # Add borders between major sections
    tab_style(
      style = cell_borders(
        sides = "right",
        color = "#BDBDBD",
        weight = px(2)
      ),
      locations = cells_body(
        columns = any_of(c("rank", "revenue_alltime_us", "rpd_alltime_us", 
                          "downloads_alltime_ww", "mau_month_us", 
                          "retention_60d_us", "genders_us"))
      )
    ) %>%
    
    # Style rank column with center alignment
    tab_style(
      style = list(
        cell_fill(color = "#F5F5F5"),
        cell_text(weight = "bold", align = "center")
      ),
      locations = cells_body(columns = rank)
    ) %>%
    
    # Center align rank header too
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = rank)
    ) %>%
    
    # Highlight top 3 games
    tab_style(
      style = list(
        cell_fill(color = "#FFF8E1"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = 1:3
      )
    ) %>%

    # Add source note
    tab_source_note(
      source_note = "Source: Sensor Tower API"
    )


print(gt_table)

# --- Save GT table as PNG ---

# Create output directory for images
output_dir <- "inst/images"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the gt table as PNG
gt_table %>%
  gtsave(
    filename = file.path(output_dir, "rpg_analytics_dashboard.png"),
    vwidth = 1400,
    vheight = 900
  )

cat("\nGT table saved successfully as PNG!\n")
cat("File saved to:", file.path(output_dir, "rpg_analytics_dashboard.png"), "\n")

# --- Create Additional Visualizations ---

# 1. Revenue vs MAU Scatter Plot
revenue_mau_plot <- top_rpgs %>%
  slice_head(n = 15) %>%
  mutate(
    app_name_short = str_trunc(unified_app_name, 20, ellipsis = "..."),
    revenue_millions = revenue_180d_ww / 1000000,
    mau_millions = mau_month_ww / 1000000
  ) %>%
  ggplot(aes(x = mau_millions, y = revenue_millions)) +
  geom_point(aes(size = rpd_alltime_ww), alpha = 0.7, color = "#8B4513") +
  geom_text(aes(label = app_name_short), 
            size = 3, 
            hjust = 0, 
            vjust = -1,
            check_overlap = TRUE) +
  scale_x_continuous(labels = scales::comma_format(suffix = "M"),
                     expand = expansion(mult = c(0.1, 0.2))) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M"),
                     expand = expansion(mult = c(0.1, 0.2))) +
  scale_size_continuous(range = c(4, 12), 
                        name = "RPD (USD)",
                        labels = scales::dollar_format()) +
  labs(
    title = "Top Role Playing Games: Revenue vs Monthly Active Users",
    subtitle = "180-day revenue vs current MAU (worldwide, bubble size = RPD)",
    x = "Monthly Active Users (Millions)",
    y = "180-Day Revenue (USD Millions)",
    caption = "Data source: Sensor Tower API"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Save the plot
ggsave(
  filename = file.path(output_dir, "rpg_revenue_mau_analysis.png"),
  plot = revenue_mau_plot,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 2. Retention Comparison for Top 10 Games
retention_plot <- top_rpgs %>%
  slice_head(n = 10) %>%
  select(unified_app_name, retention_1d_us, retention_7d_us, retention_30d_us) %>%
  filter(!is.na(retention_1d_us)) %>%  # Filter out games with no retention data
  mutate(app_name_short = str_trunc(unified_app_name, 25, ellipsis = "...")) %>%
  pivot_longer(
    cols = c(retention_1d_us, retention_7d_us, retention_30d_us),
    names_to = "retention_period",
    values_to = "retention_rate"
  ) %>%
  mutate(
    retention_period = case_when(
      retention_period == "retention_1d_us" ~ "Day 1",
      retention_period == "retention_7d_us" ~ "Day 7",
      retention_period == "retention_30d_us" ~ "Day 30"
    ),
    retention_period = factor(retention_period, levels = c("Day 1", "Day 7", "Day 30"))
  ) %>%
  ggplot(aes(x = retention_period, y = retention_rate, group = app_name_short)) +
  geom_line(aes(color = app_name_short), size = 1.2, alpha = 0.8) +
  geom_point(aes(color = app_name_short), size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 100)) +
  scale_color_brewer(palette = "Set3", name = "Game") +
  labs(
    title = "User Retention Curves for Top Mobile Games",
    subtitle = "Retention rates at Day 1, 7, and 30 (US market)",
    x = "Days Since Install",
    y = "Retention Rate (%)",
    caption = "Data source: Sensor Tower API"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Save retention plot
ggsave(
  filename = file.path(output_dir, "rpg_retention_curves.png"),
  plot = retention_plot,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

# 3. Revenue Per Download (RPD) Analysis
rpd_plot <- top_rpgs %>%
  filter(!is.na(rpd_alltime_ww)) %>%
  slice_head(n = 10) %>%
  mutate(
    app_name_short = str_trunc(unified_app_name, 35, ellipsis = "..."),
    revenue_billions = revenue_180d_ww / 1000000000
  ) %>%
  arrange(desc(rpd_alltime_ww)) %>%
  ggplot(aes(x = reorder(app_name_short, rpd_alltime_ww), y = rpd_alltime_ww)) +
  geom_col(aes(fill = revenue_billions), width = 0.7) +
  geom_text(aes(label = dollar(rpd_alltime_ww, accuracy = 0.01)), 
            hjust = -0.1, 
            size = 3.5) +
  scale_fill_gradient(low = "#A8DADC", high = "#1D3557", 
                      name = "180d Revenue\n(Billions USD)") +
  scale_y_continuous(labels = scales::dollar_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title = "Revenue Per Download (RPD) - Top Monetizing Games",
    subtitle = "All-time RPD in worldwide market",
    x = "",
    y = "Revenue Per Download (USD)",
    caption = "Data source: Sensor Tower API"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save RPD plot
ggsave(
  filename = file.path(output_dir, "rpg_rpd_analysis.png"),
  plot = rpd_plot,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("\nAll visualizations created successfully!\n")
cat("Files saved to:", output_dir, "\n")
cat("- rpg_analytics_dashboard.png (GT table)\n")
cat("- rpg_revenue_mau_analysis.png\n")
cat("- rpg_retention_curves.png\n")
cat("- rpg_rpd_analysis.png\n")
