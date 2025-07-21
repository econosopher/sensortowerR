# Social Casino Analysis - Robust Version with Error Handling
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

# Helper function for safe column selection
safe_select <- function(data, ...) {
  cols <- c(...)
  available <- intersect(cols, names(data))
  if (length(available) == 0) {
    warning("None of the requested columns found: ", paste(cols, collapse = ", "))
    return(data)
  }
  return(dplyr::select(data, dplyr::all_of(available)))
}

# Helper function to find revenue/download columns
find_metric_column <- function(data, metric, time_period, region = NULL) {
  # Build possible column names
  patterns <- c()
  if (!is.null(region)) {
    patterns <- c(patterns, paste0(metric, "_", time_period, "_", tolower(region)))
    patterns <- c(patterns, paste0(metric, "_", time_period, "_", toupper(region)))
  }
  # Also check without region
  patterns <- c(patterns, paste0(metric, "_", time_period))
  
  # Find first matching column
  for (pattern in patterns) {
    if (pattern %in% names(data)) {
      return(pattern)
    }
  }
  
  # Try partial matching
  cols <- grep(paste0(metric, ".*", time_period), names(data), value = TRUE)
  if (length(cols) > 0) {
    return(cols[1])
  }
  
  return(NULL)
}

# First, let's find Monopoly Go and its category
cat("Looking up Monopoly Go to find Social Casino category...\n")
monopoly_info <- tryCatch({
  st_app_info("Monopoly Go", return_all_fields = TRUE, limit = 1)
}, error = function(e) {
  stop("Failed to fetch Monopoly Go info: ", e$message)
})

# Extract the category information
if (nrow(monopoly_info) > 0) {
  category_info <- monopoly_info %>%
    tidyr::unnest(category_details)
  
  cat("\nMonopoly Go categories:\n")
  # Use safe column selection
  app_name_col <- find_column(category_info, "app_name|unified_app_name|name")
  if (!is.null(app_name_col)) {
    print(category_info %>% safe_select(app_name_col, "category_id", "category_name"))
  }
  
  # Use whatever category Monopoly Go is in
  # Don't assume it's specifically "Casino" - it could be in a broader category
  if (nrow(category_info) > 0) {
    # Use the first category (usually the main category)
    category_id <- category_info$category_id[1]
    category_name <- category_info$category_name[1]
    cat("\nMonopoly Go is in category:", category_name, "(ID:", category_id, ")\n")
    cat("Using this category for analysis\n")
  } else {
    stop("No category information found for Monopoly Go")
  }
} else {
  stop("Could not find Monopoly Go. Please check the game name.")
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
  top_social_casino <- tryCatch({
    st_top_charts(
      measure = "revenue",
      category = category_id,
      regions = "US",  # US-only focus
      limit = 10
    )
  }, error = function(e) {
    stop("Failed to fetch top charts: ", e$message)
  })
  
  # Save to cache
  saveRDS(top_social_casino, cache_file)
  cat("Data cached to:", cache_file, "\n")
}

# Validate data structure
cat("\nValidating data structure...\n")

# Find app name column
app_name_col <- find_column(top_social_casino, "name", 
                           prefer = c("unified_app_name", "app_name", "app.name"))
if (is.null(app_name_col)) {
  stop("No app name column found in data")
}

# Find revenue columns
revenue_180d_col <- find_metric_column(top_social_casino, "revenue", "180d")
revenue_30d_col <- find_metric_column(top_social_casino, "revenue", "30d")
downloads_30d_col <- find_metric_column(top_social_casino, "downloads", "30d")

if (is.null(revenue_180d_col)) {
  warning("No 180-day revenue column found")
}

# Display what we found
cat("\nData structure:\n")
cat("- App name column:", app_name_col, "\n")
cat("- 180d revenue column:", revenue_180d_col %||% "NOT FOUND", "\n")
cat("- 30d revenue column:", revenue_30d_col %||% "NOT FOUND", "\n")
cat("- 30d downloads column:", downloads_30d_col %||% "NOT FOUND", "\n")

# Show top games by revenue
cat("\nTop", category_name, "games by revenue:\n")
if (!is.null(revenue_180d_col)) {
  display_cols <- c(app_name_col, revenue_180d_col, revenue_30d_col, downloads_30d_col)
  display_cols <- intersect(display_cols, names(top_social_casino))
  
  print(top_social_casino %>% 
    safe_select(display_cols) %>%
    head(10))
}

# Check if Monopoly Go is in the list
monopoly_go <- top_social_casino %>%
  filter(grepl("Monopoly", .data[[app_name_col]], ignore.case = TRUE))

if (nrow(monopoly_go) > 0) {
  cat("\nFound Monopoly Go in the data!\n")
  rank <- which(top_social_casino[[app_name_col]] == monopoly_go[[app_name_col]][1])
  cat("Rank:", rank, "\n")
} else {
  cat("\nMonopoly Go not found in top 20. Proceeding with analysis of all top games.\n")
}

# Create dashboard with robust column handling
cat("\nCreating Social Casino dashboard with US metrics...\n")

# Check which columns are available for the dashboard
dashboard_result <- tryCatch({
  st_gt_dashboard(
    top_social_casino,
    title = paste("Top", category_name, "- US Market"),
    subtitle = paste("Ranked by 180-day revenue (", 
                     format(Sys.Date() - 180, "%b %Y"), "-",
                     format(Sys.Date(), "%b %Y"), ") |",
                     "Category:", category_name, "(#", category_id, ")"),
    ranking_metric = revenue_180d_col %||% revenue_30d_col %||% "revenue",
    retention_region = "us",
    save_path = "inst/images/social_casino_dashboard_robust.png"
  )
}, error = function(e) {
  warning("Dashboard creation failed: ", e$message)
  NULL
})

if (!is.null(dashboard_result)) {
  cat("Dashboard created successfully!\n")
  cat("Saved to: inst/images/social_casino_dashboard_robust.png\n")
}

# Create individual visualizations with error handling
# 1. Revenue Chart
cat("\nCreating revenue trend analysis...\n")

if (!is.null(revenue_30d_col) && revenue_30d_col %in% names(top_social_casino)) {
  revenue_plot <- tryCatch({
    top_social_casino %>%
      head(10) %>%
      mutate(
        game_short = stringr::str_trunc(.data[[app_name_col]], 20, "right"),
        revenue_millions = .data[[revenue_30d_col]] / 1e6
      ) %>%
      ggplot(aes(x = reorder(game_short, revenue_millions), y = revenue_millions)) +
      geom_col(fill = "#FF6600") +
      coord_flip() +
      scale_y_continuous(labels = scales::dollar_format(suffix = "M")) +
      labs(
        title = paste("Top", category_name, "by Monthly Revenue"),
        subtitle = "US Market Focus - Last 30 Days",
        x = "",
        y = "Revenue (Millions USD)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "gray40", size = 12),
        panel.grid.major.y = element_blank()
      )
  }, error = function(e) {
    warning("Revenue plot failed: ", e$message)
    NULL
  })
  
  if (!is.null(revenue_plot)) {
    ggsave(
      "inst/images/social_casino_revenue_robust.png",
      revenue_plot,
      width = 10,
      height = 6,
      dpi = 300
    )
    cat("Revenue chart saved successfully\n")
  }
} else {
  warning("Cannot create revenue chart - revenue column not found")
}

# 2. Retention Analysis with robust column checking
cat("\nCreating retention analysis...\n")

# Find retention columns
retention_cols <- grep("retention_[0-9]+d_(us|ww)", names(top_social_casino), value = TRUE)

if (length(retention_cols) >= 3) {
  retention_plot <- tryCatch({
    retention_data <- top_social_casino %>%
      safe_select(app_name_col, retention_cols) %>%
      filter(!is.na(.data[[retention_cols[1]]])) %>%
      head(10) %>%
      pivot_longer(
        cols = all_of(retention_cols),
        names_to = "retention_day",
        values_to = "retention_rate"
      ) %>%
      mutate(
        day_num = as.numeric(gsub("retention_([0-9]+)d_.*", "\\1", retention_day)),
        game_short = stringr::str_trunc(.data[[app_name_col]], 20, "right")
      ) %>%
      filter(day_num %in% c(1, 7, 30))
    
    ggplot(retention_data, aes(x = day_num, y = retention_rate, 
                               color = game_short, group = game_short)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = c(1, 7, 30), labels = c("Day 1", "Day 7", "Day 30")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_color_brewer(palette = "Set3") +
      labs(
        title = paste(category_name, "- Retention Curves"),
        subtitle = "Retention Rates Over Time",
        x = "Days After Install",
        y = "Retention Rate (%)",
        color = "Game"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "gray40", size = 12),
        legend.position = "right"
      )
  }, error = function(e) {
    warning("Retention plot failed: ", e$message)
    NULL
  })
  
  if (!is.null(retention_plot)) {
    ggsave(
      "inst/images/social_casino_retention_robust.png",
      retention_plot,
      width = 12,
      height = 6,
      dpi = 300
    )
    cat("Retention chart saved successfully\n")
  }
} else {
  warning("Cannot create retention chart - insufficient retention columns found")
}

cat("\nAnalysis completed!\n")
cat("\nNote: This robust version handles column variations and missing data gracefully.\n")
cat("Check warnings above for any data issues encountered.\n")