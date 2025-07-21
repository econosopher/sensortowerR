# Social Casino Analysis - Starting with Monopoly Go
# US Market Focus

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  char = c(
    "devtools",
    "gt",
    "gtExtras",
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

# First, let's find Monopoly Go and its category
cat("Looking up Monopoly Go to find Social Casino category...\n")
monopoly_info <- st_app_info("Monopoly Go", return_all_fields = TRUE, limit = 1)

# Extract the category information
if (nrow(monopoly_info) > 0 && "category_details" %in% names(monopoly_info)) {
  # Extract categories from the list column
  all_categories <- monopoly_info$category_details[[1]]
  
  if (is.data.frame(all_categories) || is_tibble(all_categories)) {
    cat("\nMonopoly Go has", nrow(all_categories), "categories:\n")
    print(all_categories)
    
    # Handle multiple categories
    if (nrow(all_categories) > 1) {
      cat("\nApp has multiple categories. Options:\n")
      for (i in 1:nrow(all_categories)) {
        cat(i, ". ", all_categories$category_name[i], " (ID: ", all_categories$category_id[i], ")\n", sep = "")
      }
      
      # Use the primary Games category if available, otherwise use first category
      games_category <- all_categories %>%
        filter(category_id == "6014" | category_name == "Games")
      
      if (nrow(games_category) > 0) {
        category_id <- games_category$category_id[1]
        category_name <- games_category$category_name[1]
        cat("\nUsing primary Games category for analysis\n")
      } else {
        # Use first category as fallback
        category_id <- all_categories$category_id[1]
        category_name <- all_categories$category_name[1]
        cat("\nUsing first category for analysis\n")
      }
    } else {
      # Single category
      category_id <- all_categories$category_id[1]
      category_name <- all_categories$category_name[1]
    }
    
    cat("\nSelected category:", category_name, "(ID:", category_id, ")\n")
  } else {
    stop("Unexpected category_details structure")
  }
} else {
  stop("Could not find Monopoly Go or category information. Please check the game name.")
}

# Check for cached data first
cache_file <- "inst/cache/top_social_casino_cache.rds"
cache_dir <- dirname(cache_file)

# Create cache directory if it doesn't exist
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Load cached data if it exists, otherwise fetch from API
if (file.exists(cache_file)) {
  cat("\nUsing cached data from:", cache_file, "\n")
  top_social_casino <- readRDS(cache_file)
} else {
  cat("\nFetching top", category_name, "games from API (US focus)...\n")
  
  # Fetch top games in this category with US focus
  top_social_casino <- st_top_charts(
    measure = "revenue",
    category = category_id,
    regions = "US",  # US-only focus
    limit = 20
  )
  
  # Save to cache
  saveRDS(top_social_casino, cache_file)
  cat("Data cached to:", cache_file, "\n")
}

# Let's first look at what we have
cat("\nTop", category_name, "games by revenue:\n")
# Check available columns
revenue_cols <- grep("revenue", names(top_social_casino), value = TRUE)
download_cols <- grep("download", names(top_social_casino), value = TRUE)
cat("Available revenue columns:", revenue_cols[1:min(5, length(revenue_cols))], "...\n")
cat("Available download columns:", download_cols[1:min(5, length(download_cols))], "...\n")

# Select appropriate columns based on what's available
if ("revenue_180d_us" %in% names(top_social_casino)) {
  print(top_social_casino %>% 
    select(unified_app_name, revenue_180d_us, revenue_30d_us, downloads_30d_us) %>%
    head(10))
} else if ("revenue_180d_ww" %in% names(top_social_casino)) {
  print(top_social_casino %>% 
    select(unified_app_name, revenue_180d_ww, revenue_30d_ww, downloads_30d_ww) %>%
    head(10))
}

# Check if Monopoly Go is in the list
# First verify we have the unified_app_name column
if (!"unified_app_name" %in% names(top_social_casino)) {
  warning("unified_app_name column not found, skipping Monopoly Go search")
} else {
  monopoly_go <- top_social_casino %>%
    filter(grepl("Monopoly", unified_app_name, ignore.case = TRUE))
  
  if (nrow(monopoly_go) > 0) {
    cat("\nFound Monopoly Go in the data!\n")
    cat("Rank:", which(top_social_casino$unified_app_name == monopoly_go$unified_app_name[1]), "\n")
    # Use available columns
    available_cols <- c("unified_app_name")
    if ("revenue_180d_ww" %in% names(monopoly_go)) available_cols <- c(available_cols, "revenue_180d_ww")
    if ("revenue_30d_ww" %in% names(monopoly_go)) available_cols <- c(available_cols, "revenue_30d_ww")
    if ("retention_1d_us" %in% names(monopoly_go)) available_cols <- c(available_cols, "retention_1d_us")
    
    print(monopoly_go %>% select(all_of(available_cols)))
  } else {
    cat("\nMonopoly Go not found in top 20. Proceeding with analysis of all top games.\n")
  }
}

# Create a comprehensive dashboard using the new st_gt_dashboard function
# Focus on US metrics
cat("\nCreating Social Casino dashboard with US metrics...\n")

# The data already has the columns we need - no mapping required
# When regions="US" is specified, we still get WW columns for revenue/downloads
# But we have US columns for retention and user metrics

# Create the dashboard
social_casino_dashboard <- st_gt_dashboard(
  top_social_casino,
  title = paste("Top", category_name, "on Mobile"),
  subtitle = paste("Performance Dashboard -", format(Sys.Date(), "%B %Y")),
  ranking_metric = "revenue_180d_ww",
  retention_region = "us",
  save_path = "inst/images/social_casino_dashboard.png"
)

cat("\nDashboard created successfully!\n")
cat("Saved to: inst/images/social_casino_dashboard.png\n")

# Also create individual analysis charts
# 1. Revenue Trend Analysis
cat("\nCreating revenue trend analysis...\n")

revenue_plot <- top_social_casino %>%
  head(10) %>%
  mutate(
    game_short = stringr::str_trunc(unified_app_name, 20, "right"),
    revenue_millions = revenue_30d_ww / 1e6  # Using WW column
  ) %>%
  ggplot(aes(x = reorder(game_short, revenue_millions), y = revenue_millions)) +
  geom_col(fill = "#FF6600") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M")) +
  labs(
    title = paste("Top", category_name, "by Monthly Revenue"),
    subtitle = "30-day revenue in US market",
    x = "",
    y = "Revenue (Millions USD)",
    caption = "Source: Sensor Tower Store Intelligence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    panel.grid.major.y = element_blank()
  )

ggsave(
  "inst/images/social_casino_revenue_chart.png",
  revenue_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# 2. User Engagement Analysis (DAU/MAU ratio)
cat("\nCreating user engagement analysis...\n")

engagement_plot <- top_social_casino %>%
  filter(!is.na(dau_30d_us) & !is.na(mau_month_us) & mau_month_us > 0) %>%
  mutate(
    game_short = stringr::str_trunc(unified_app_name, 15, "right"),
    dau_mau_ratio = (dau_30d_us / mau_month_us) * 100
  ) %>%
  head(10) %>%
  ggplot(aes(x = reorder(game_short, dau_mau_ratio), y = dau_mau_ratio)) +
  geom_col(fill = "#9C27B0") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = paste(category_name, "- User Engagement"),
    subtitle = "DAU/MAU ratio in US market",
    x = "",
    y = "DAU/MAU Ratio (%)",
    caption = "Higher ratios indicate better daily engagement"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    panel.grid.major.y = element_blank()
  )

ggsave(
  "inst/images/social_casino_engagement_chart.png",
  engagement_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# 3. Retention Analysis
cat("\nCreating retention analysis...\n")

retention_data <- top_social_casino %>%
  select(unified_app_name, retention_1d_us, retention_7d_us, retention_30d_us) %>%
  filter(!is.na(retention_1d_us)) %>%
  head(10) %>%
  pivot_longer(
    cols = starts_with("retention"),
    names_to = "retention_day",
    values_to = "retention_rate"
  ) %>%
  mutate(
    day = case_when(
      grepl("1d", retention_day) ~ "Day 1",
      grepl("7d", retention_day) ~ "Day 7",
      grepl("30d", retention_day) ~ "Day 30"
    ),
    day_num = case_when(
      grepl("1d", retention_day) ~ 1,
      grepl("7d", retention_day) ~ 7,
      grepl("30d", retention_day) ~ 30
    ),
    game_short = stringr::str_trunc(unified_app_name, 20, "right")
  )

retention_plot <- ggplot(retention_data, aes(x = day_num, y = retention_rate, 
                                             color = game_short, group = game_short)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(1, 7, 30), labels = c("Day 1", "Day 7", "Day 30")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_brewer(palette = "Set3") +
  labs(
    title = paste(category_name, "- Retention Curves"),
    subtitle = "Retention rates at Day 1, 7, and 30 (US market)",
    x = "Days After Install",
    y = "Retention Rate (%)",
    color = "Game",
    caption = "Source: Sensor Tower Store Intelligence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 10)
  ) +
  guides(color = guide_legend(ncol = 1))

ggsave(
  "inst/images/social_casino_retention_curves.png",
  retention_plot,
  width = 12,
  height = 6,
  dpi = 300
)

# 4. Monetization Analysis - RPD comparison
cat("\nCreating monetization analysis...\n")

monetization_plot <- top_social_casino %>%
  filter(!is.na(rpd_alltime_us)) %>%
  head(10) %>%
  mutate(
    game_short = stringr::str_trunc(unified_app_name, 20, "right")
  ) %>%
  ggplot(aes(x = reorder(game_short, rpd_alltime_us), y = rpd_alltime_us)) +
  geom_col(fill = "#4CAF50") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = paste(category_name, "- Monetization Metrics"),
    subtitle = "All-time RPD in US market",
    x = "",
    y = "Revenue Per Download ($)",
    caption = "Source: Sensor Tower Store Intelligence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    panel.grid.major.y = element_blank()
  )

ggsave(
  "inst/images/social_casino_monetization_chart.png",
  monetization_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\nAll analyses completed successfully!\n")
cat("\nGenerated files:\n")
cat("- inst/images/social_casino_dashboard.png\n")
cat("- inst/images/social_casino_revenue_chart.png\n")
cat("- inst/images/social_casino_engagement_chart.png\n")
cat("- inst/images/social_casino_retention_curves.png\n")
cat("- inst/images/social_casino_monetization_chart.png\n")