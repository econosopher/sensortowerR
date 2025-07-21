# Squad RPG Analysis with FiveThirtyEight Theme
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

# Function to get and cache app icons
get_app_icon <- function(app_id, app_name, cache_dir = "inst/images/app_icons") {
  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Clean app name for filename (remove special characters)
  clean_name <- gsub("[^A-Za-z0-9]", "_", app_name)
  icon_filename <- paste0(clean_name, ".png")
  icon_path <- file.path(cache_dir, icon_filename)
  
  # Check if icon already exists in cache
  if (file.exists(icon_path)) {
    return(icon_path)
  }
  
  # If not in cache, we would download here
  # For now, return a placeholder or NULL
  # In production, this would call an API to get the icon
  # Example: download.file(icon_url, icon_path)
  
  # For demonstration, let's create a placeholder
  # In real implementation, this would be the download logic
  return(NULL)
}

# Function to convert country code to flag emoji
country_to_flag <- function(country_code) {
  if (is.na(country_code) || country_code == "") return("")
  
  # Convert to uppercase
  country_code <- toupper(country_code)
  
  # Map of country codes to names and emojis
  country_map <- list(
    "JP" = "🇯🇵 Japan",
    "HK" = "🇭🇰 Hong Kong",
    "US" = "🇺🇸 USA",
    "CN" = "🇨🇳 China",
    "KR" = "🇰🇷 South Korea",
    "TW" = "🇹🇼 Taiwan",
    "GB" = "🇬🇧 UK",
    "DE" = "🇩🇪 Germany",
    "FR" = "🇫🇷 France",
    "CA" = "🇨🇦 Canada",
    "AU" = "🇦🇺 Australia",
    "BR" = "🇧🇷 Brazil",
    "IN" = "🇮🇳 India",
    "MX" = "🇲🇽 Mexico",
    "ES" = "🇪🇸 Spain",
    "IT" = "🇮🇹 Italy",
    "RU" = "🇷🇺 Russia",
    "NL" = "🇳🇱 Netherlands",
    "SE" = "🇸🇪 Sweden",
    "SG" = "🇸🇬 Singapore"
  )
  
  # Return mapped value or just the code if not found
  if (country_code %in% names(country_map)) {
    return(country_map[[country_code]])
  } else {
    return(country_code)
  }
}

# Use well-known Role Playing category ID
role_playing_id <- 7014

# Check for cached data first
cache_file <- "inst/cache/top_rpgs_cache.rds"
cache_dir <- dirname(cache_file)

# Create cache directory if it doesn't exist
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Load cached data if it exists, otherwise fetch from API
if (file.exists(cache_file)) {
  cat("Using cached data from:", cache_file, "\n")
  top_rpgs <- readRDS(cache_file)
} else {
  cat("No cache found. Would fetch from API (currently disabled to save API calls)\n")
  cat("Please run with explicit user permission to fetch new data.\n")
  stop("No cached data available. Run with user permission to fetch from API.")
  
  # This code would only run with user permission:
  # top_rpgs <- st_top_charts(
  #   measure = "revenue",
  #   category = role_playing_id,
  #   limit = 20
  # )
  # saveRDS(top_rpgs, cache_file)
  # cat("Data cached to:", cache_file, "\n")
}

# Prepare data with proper column selection
table_data <- top_rpgs %>%
  mutate(
    rank = row_number(),
    # Add icon paths (will be NULL for now until icons are downloaded)
    icon_path = purrr::map2_chr(unified_app_id, unified_app_name, 
                        ~get_app_icon(.x, .y) %||% ""),
    # Map gender column
    genders_ww = `entities.custom_tags.Genders (Last Quarter, WW)`,
    # Calculate years since launch first
    years_since_launch = case_when(
      !is.na(release_date_ww) ~ {
        months_diff <- as.numeric(difftime(Sys.Date(), release_date_ww, units = "days")) / 30.44
        years <- floor(months_diff / 12)
        months <- round(months_diff %% 12)
        case_when(
          years == 0 & months == 1 ~ "1 month",
          years == 0 ~ paste0(months, " months"),
          years == 1 & months == 0 ~ "1 year",
          years == 1 & months == 1 ~ "1 year, 1 month",
          years == 1 ~ paste0("1 year, ", months, " months"),
          months == 0 ~ paste0(years, " years"),
          months == 1 ~ paste0(years, " years, 1 month"),
          TRUE ~ paste0(years, " years, ", months, " months")
        )
      },
      TRUE ~ "—"
    )
  ) %>%
  select(
    rank,
    icon_path,
    app_name = unified_app_name,
    
    # Ranking KPI (180-day revenue) - moved to front
    revenue_180d_ww,
    
    # Demographics - moved after game name
    any_of(c("age_ww", "genders_ww")),
    years_since_launch,
    
    # Top revenue country - moved to demographics
    any_of(c("most_popular_country_revenue", "entities.custom_tags.Most Popular Country by Revenue")),
    
    # Other revenue metrics
    any_of(c("revenue_30d_ww", "revenue_alltime_ww")),
    
    # RPD
    any_of(c("rpd_alltime_ww")),
    
    # Downloads
    any_of(c("downloads_30d_ww", "downloads_180d_ww", "downloads_alltime_ww")),
    
    # Active users
    any_of(c("dau_30d_ww", "wau_4w_ww", "mau_month_ww")),
    
    # Retention - US data with available time periods
    any_of(c("retention_1d_us", "retention_7d_us", "retention_30d_us", "retention_60d_us"))
  ) %>%
  # Fix retention values (multiply by 100 if they're in decimal form like 0.3)
  mutate(
    across(any_of(c("retention_1d_us", "retention_7d_us", "retention_30d_us", "retention_60d_us")), 
           ~if_else(. < 1, . * 100, .))
  )

# Create FiveThirtyEight-styled GT table
fivethirtyeight_table <- table_data %>%
  gt() %>%
  
  # Apply the authentic 538 theme from gtExtras
  gt_theme_538() %>%
  
  # Set consistent row heights - reduced by half
  tab_options(
    data_row.padding = px(4),
    row.striping.include_table_body = FALSE
  ) %>%
  
  # Header with 538-style typography
  tab_header(
    title = "Top Role-Playing Games on Mobile",
    subtitle = "Revenue, engagement, and retention metrics for leading Squad RPG titles"
  ) %>%
  
  # Column labels with better naming
  cols_label(
    rank = "",
    icon_path = "",
    app_name = "Game",
    revenue_180d_ww = "180-Day Revenue (Ranking KPI)",
    age_ww = "Avg Age",
    genders_ww = "Gender",
    years_since_launch = "Years Live",
    `entities.custom_tags.Most Popular Country by Revenue` = "Top Country (Rev)",
    revenue_30d_ww = "30-Day Revenue",
    revenue_alltime_ww = "Lifetime Revenue",
    rpd_alltime_ww = "Revenue per Download",
    downloads_30d_ww = "30-Day Downloads",
    downloads_180d_ww = "180-Day Downloads",
    downloads_alltime_ww = "Lifetime Downloads",
    dau_30d_ww = "DAU (30d avg)",
    wau_4w_ww = "WAU (4w avg)",
    mau_month_ww = "MAU (monthly avg)",
    retention_1d_us = "Day 1",
    retention_7d_us = "Day 7", 
    retention_30d_us = "Day 30",
    retention_60d_us = "Day 60"
  ) %>%
  
  # Hide columns we don't need
  cols_hide(columns = any_of(c("icon_path", "retention_trend", "release_date_ww"))) %>%
  
  # Column groups with 538-style headers - reorganized
  tab_spanner(
    label = "RANKING KPI",
    columns = c("revenue_180d_ww")
  ) %>%
  
  tab_spanner(
    label = "DEMOGRAPHICS",
    columns = any_of(c("age_ww", "genders_ww", "years_since_launch", 
                      "entities.custom_tags.Most Popular Country by Revenue"))
  ) %>%
  
  tab_spanner(
    label = "REVENUE",
    columns = any_of(c("revenue_30d_ww", "revenue_alltime_ww", "rpd_alltime_ww"))
  ) %>%
  
  tab_spanner(
    label = "DOWNLOADS",
    columns = any_of(c("downloads_30d_ww", "downloads_180d_ww", "downloads_alltime_ww"))
  ) %>%
  
  tab_spanner(
    label = "ENGAGEMENT",
    columns = any_of(c("dau_30d_ww", "wau_4w_ww", "mau_month_ww"))
  ) %>%
  
  tab_spanner(
    label = "RETENTION (US)",
    columns = any_of(c("retention_1d_us", "retention_7d_us", "retention_30d_us", "retention_60d_us"))
  ) %>%
  
  # Number formatting
  fmt_number(
    columns = rank,
    decimals = 0
  ) %>%
  
  fmt_number(
    columns = any_of(c("dau_30d_ww", "wau_4w_ww", "mau_month_ww",
                      "downloads_30d_ww", "downloads_180d_ww", 
                      "downloads_alltime_ww")),
    decimals = 0,
    suffixing = TRUE
  ) %>%
  
  fmt_currency(
    columns = any_of(c("revenue_180d_ww", "revenue_30d_ww", 
                      "revenue_alltime_ww")),
    currency = "USD",
    decimals = 0,
    suffixing = TRUE
  ) %>%
  
  fmt_currency(
    columns = any_of(c("rpd_alltime_ww")),
    currency = "USD",
    decimals = 2
  ) %>%
  
  fmt_number(
    columns = any_of(c("age_ww")),
    decimals = 0
  ) %>%
  
  # Years since launch already formatted as text
  # fmt_date removed
  
  sub_missing(
    columns = everything(),
    missing_text = "—"
  ) %>%
  
  # Add gender formatting with colors and emojis
  text_transform(
    locations = cells_body(columns = genders_ww),
    fn = function(x) {
      purrr::map_chr(x, function(gender) {
        if (is.na(gender) || gender == "") return("—")
        
        # Parse the gender percentage (format is usually "XX% Male")
        male_match <- regexpr("[0-9]+(?=% Male)", gender, perl = TRUE)
        if (male_match > 0) {
          male_pct <- as.numeric(regmatches(gender, male_match))
        } else {
          # Try alternative format
          male_pct <- as.numeric(gsub("[^0-9]", "", gender))
        }
        
        # Calculate female percentage
        female_pct <- 100 - male_pct
        
        # Create styled HTML with emojis and colors
        sprintf(
          '<div style="font-size: 12px;">
            <span style="color: #4A90E2; font-weight: 500;">♂ %d%%</span>
            <span style="color: #E91E63; font-weight: 500; margin-left: 8px;">♀ %d%%</span>
          </div>',
          male_pct,
          female_pct
        )
      })
    }
  ) %>%
  
  # Apply consistent color gradients to all numeric columns
  # Revenue columns
  gt_color_rows(
    revenue_30d_ww,
    palette = c("#FFFFFF", "#FF6600"),
    domain = NULL
  ) %>%
  
  gt_color_rows(
    revenue_alltime_ww,
    palette = c("#FFFFFF", "#FF6600"),
    domain = NULL
  ) %>%
  
  # Downloads columns
  gt_color_rows(
    downloads_180d_ww,
    palette = c("#FFFFFF", "#008FD5"),
    domain = NULL
  ) %>%
  
  gt_color_rows(
    downloads_alltime_ww,
    palette = c("#FFFFFF", "#008FD5"),
    domain = NULL
  ) %>%
  
  # Engagement columns
  gt_color_rows(
    dau_30d_ww,
    palette = c("#FFFFFF", "#9C27B0"),
    domain = NULL
  ) %>%
  
  gt_color_rows(
    wau_4w_ww,
    palette = c("#FFFFFF", "#9C27B0"),
    domain = NULL
  ) %>%
  
  gt_color_rows(
    mau_month_ww,
    palette = c("#FFFFFF", "#9C27B0"),
    domain = NULL
  ) %>%
  
  # Retention columns
  gt_color_rows(
    retention_1d_us,
    palette = c("#FFEBEE", "#4CAF50"),
    domain = c(0, 100)
  ) %>%
  
  gt_color_rows(
    retention_7d_us,
    palette = c("#FFEBEE", "#4CAF50"),
    domain = c(0, 100)
  ) %>%
  
  gt_color_rows(
    retention_30d_us,
    palette = c("#FFEBEE", "#4CAF50"),
    domain = c(0, 100)
  ) %>%
  
  gt_color_rows(
    retention_60d_us,
    palette = c("#FFEBEE", "#4CAF50"),
    domain = c(0, 100)
  ) %>%
  
  
  # Add bar charts with values to downloads columns
  text_transform(
    locations = cells_body(columns = downloads_30d_ww),
    fn = function(x) {
      # Get values from the column
      values <- as.numeric(gsub("[^0-9.]", "", x))
      # Handle suffixes
      mult <- ifelse(grepl("K", x), 1000, 
              ifelse(grepl("M", x), 1000000,
              ifelse(grepl("B", x), 1000000000, 1)))
      values <- values * mult
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map2_chr(x, values, function(label, val) {
        if (is.na(val)) return("—")
        
        # Calculate bar width as percentage
        bar_width <- (val / max_val) * 100
        
        # Create inline bar chart with value
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #008FD5; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          label
        )
      })
    }
  ) %>%
  
  # Color code the RPD values
  gt_color_rows(
    rpd_alltime_ww,
    palette = c("#E8F5E9", "#4CAF50"),
    domain = c(0, 50)
  ) %>%
  
  # Style the rank column with circles
  tab_style(
    style = list(
      cell_text(
        size = px(20),
        weight = "bold",
        color = "#666666"
      )
    ),
    locations = cells_body(columns = rank)
  ) %>%
  
  # Highlight top 3 games
  tab_style(
    style = list(
      cell_fill(color = "#F2F2F2"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = 1:3,
      columns = c(app_name)
    )
  ) %>%
  
  # Add country flags to top revenue country column
  text_transform(
    locations = cells_body(columns = any_of(c("most_popular_country_revenue", 
                                              "entities.custom_tags.Most Popular Country by Revenue"))),
    fn = function(x) {
      purrr::map_chr(x, country_to_flag)
    }
  ) %>%
  
  # Add icons for top performers
  text_transform(
    locations = cells_body(columns = rank, rows = 1),
    fn = function(x) paste0(x, " 🥇")
  ) %>%
  text_transform(
    locations = cells_body(columns = rank, rows = 2),
    fn = function(x) paste0(x, " 🥈")
  ) %>%
  text_transform(
    locations = cells_body(columns = rank, rows = 3),
    fn = function(x) paste0(x, " 🥉")
  ) %>%
  
  # Add footnotes
  tab_footnote(
    footnote = "Data represents worldwide metrics unless otherwise specified",
    locations = cells_column_labels(columns = revenue_180d_ww)
  ) %>%
  
  tab_footnote(
    footnote = "All engagement metrics show average values over their respective time periods",
    locations = cells_column_spanners(spanners = "ENGAGEMENT")
  ) %>%
  
  
  # Source note
  tab_source_note(
    source_note = md("**Source:** Sensor Tower Store Intelligence | **Updated:** July 2025")
  ) %>%
  
  # Style ranking KPI column with better font
  tab_style(
    style = list(
      cell_text(
        font = "Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
        size = px(14),
        weight = "600"
      )
    ),
    locations = cells_body(columns = revenue_180d_ww)
  ) %>%
  
  # Style other revenue columns
  tab_style(
    style = list(
      cell_text(
        font = "Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
        size = px(13),
        weight = "500"
      )
    ),
    locations = cells_body(columns = any_of(c("revenue_30d_ww", "revenue_alltime_ww", "rpd_alltime_ww")))
  ) %>%
  
  # Style numeric columns
  tab_style(
    style = list(
      cell_text(
        font = "Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
        size = px(13)
      )
    ),
    locations = cells_body(columns = any_of(c("downloads_30d_ww", "downloads_180d_ww", 
                                              "downloads_alltime_ww", "dau_30d_ww", "wau_4w_ww", "mau_month_ww")))
  ) %>%
  
  # Make game names stand out
  tab_style(
    style = list(
      cell_text(
        size = px(14),
        weight = "600"
      )
    ),
    locations = cells_body(columns = app_name)
  )

# Create output directory if it doesn't exist
output_dir <- "inst/images"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the table with FiveThirtyEight styling
fivethirtyeight_table %>%
  gtsave(
    filename = file.path(output_dir, "rpg_analytics_dashboard.png"),
    vwidth = 1400,
    vheight = 900
  )

cat("\nFiveThirtyEight-themed dashboard created successfully!\n")
cat("File saved to:", file.path(output_dir, "rpg_analytics_dashboard.png"), "\n")