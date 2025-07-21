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
  
  # Extended map of country codes to flag emojis
  flags <- c(
    "US" = "🇺🇸", "GB" = "🇬🇧", "CA" = "🇨🇦", "AU" = "🇦🇺", "DE" = "🇩🇪",
    "FR" = "🇫🇷", "IT" = "🇮🇹", "ES" = "🇪🇸", "NL" = "🇳🇱", "SE" = "🇸🇪",
    "NO" = "🇳🇴", "DK" = "🇩🇰", "FI" = "🇫🇮", "PL" = "🇵🇱", "RU" = "🇷🇺",
    "BR" = "🇧🇷", "MX" = "🇲🇽", "AR" = "🇦🇷", "CL" = "🇨🇱", "CO" = "🇨🇴",
    "JP" = "🇯🇵", "KR" = "🇰🇷", "CN" = "🇨🇳", "TW" = "🇹🇼", "HK" = "🇭🇰",
    "SG" = "🇸🇬", "MY" = "🇲🇾", "TH" = "🇹🇭", "ID" = "🇮🇩", "PH" = "🇵🇭",
    "VN" = "🇻🇳", "IN" = "🇮🇳", "PK" = "🇵🇰", "BD" = "🇧🇩", "LK" = "🇱🇰",
    "AE" = "🇦🇪", "SA" = "🇸🇦", "EG" = "🇪🇬", "IL" = "🇮🇱", "TR" = "🇹🇷",
    "ZA" = "🇿🇦", "NG" = "🇳🇬", "KE" = "🇰🇪", "GH" = "🇬🇭", "MA" = "🇲🇦",
    "PT" = "🇵🇹", "GR" = "🇬🇷", "CH" = "🇨🇭", "AT" = "🇦🇹", "BE" = "🇧🇪",
    "CZ" = "🇨🇿", "HU" = "🇭🇺", "RO" = "🇷🇴", "BG" = "🇧🇬", "SK" = "🇸🇰",
    "HR" = "🇭🇷", "SI" = "🇸🇮", "LT" = "🇱🇹", "LV" = "🇱🇻", "EE" = "🇪🇪",
    "IE" = "🇮🇪", "NZ" = "🇳🇿", "PE" = "🇵🇪", "VE" = "🇻🇪", "EC" = "🇪🇨",
    "UY" = "🇺🇾", "PY" = "🇵🇾", "BO" = "🇧🇴", "CR" = "🇨🇷", "PA" = "🇵🇦",
    "GT" = "🇬🇹", "HN" = "🇭🇳", "SV" = "🇸🇻", "NI" = "🇳🇮", "DO" = "🇩🇴",
    "JM" = "🇯🇲", "TT" = "🇹🇹", "CU" = "🇨🇺", "PR" = "🇵🇷", "LU" = "🇱🇺",
    "IS" = "🇮🇸", "MT" = "🇲🇹", "CY" = "🇨🇾", "AL" = "🇦🇱", "MK" = "🇲🇰",
    "RS" = "🇷🇸", "BA" = "🇧🇦", "ME" = "🇲🇪", "XK" = "🇽🇰", "MD" = "🇲🇩",
    "UA" = "🇺🇦", "BY" = "🇧🇾", "AM" = "🇦🇲", "GE" = "🇬🇪", "AZ" = "🇦🇿",
    "KZ" = "🇰🇿", "UZ" = "🇺🇿", "TM" = "🇹🇲", "KG" = "🇰🇬", "TJ" = "🇹🇯"
  )
  
  # Return flag emoji with country code, or just code if not found
  if (country_code %in% names(flags)) {
    return(paste0(flags[country_code], " ", country_code))
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
  # Sort by 180-day revenue (descending) to ensure proper ranking
  arrange(desc(revenue_180d_ww)) %>%
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
        days_diff <- as.numeric(difftime(Sys.Date(), release_date_ww, units = "days"))
        years <- floor(days_diff / 365.25)
        months <- floor((days_diff %% 365.25) / 30.44)
        case_when(
          years > 0 & months > 0 ~ paste0(years, "y ", months, "m"),
          years > 0 ~ paste0(years, "y"),
          months > 0 ~ paste0(months, "m"),
          TRUE ~ "<1m"
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
    
    # Active users (moved before downloads)
    any_of(c("dau_30d_ww", "wau_4w_ww", "mau_month_ww")),
    
    # Downloads
    any_of(c("downloads_30d_ww", "downloads_180d_ww", "downloads_alltime_ww")),
    
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
    data_row.padding = px(2),
    row.striping.include_table_body = FALSE
  ) %>%
  
  # Header with 538-style typography
  tab_header(
    title = "Top Role-Playing Games on Mobile",
    subtitle = paste("Ranked by 180-day worldwide revenue (", 
                    format(Sys.Date() - 180, "%b %Y"), "-",
                    format(Sys.Date(), "%b %Y"), ") |",
                    "Category: Role Playing (#7014)")
  ) %>%
  
  # Center align all column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(everything())
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
    label = "ENGAGEMENT",
    columns = any_of(c("dau_30d_ww", "wau_4w_ww", "mau_month_ww"))
  ) %>%
  
  tab_spanner(
    label = "DOWNLOADS",
    columns = any_of(c("downloads_30d_ww", "downloads_180d_ww", "downloads_alltime_ww"))
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
    columns = any_of(c("dau_30d_ww", "wau_4w_ww", "mau_month_ww")),
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
  
  # Format retention values with % symbol
  fmt_percent(
    columns = any_of(c("retention_1d_us", "retention_7d_us", 
                      "retention_30d_us", "retention_60d_us")),
    decimals = 0,
    scale_values = FALSE  # Values are already in percentage form (e.g., 45 not 0.45)
  ) %>%
  
  # Years since launch already formatted as text
  # fmt_date removed
  
  sub_missing(
    columns = everything(),
    missing_text = "—"
  ) %>%
  
  # Format revenue columns with 2 decimals for billions using numeric values
  text_transform(
    locations = cells_body(columns = revenue_30d_ww),
    fn = function(x) {
      # Use the numeric values directly
      idx <- seq_along(x)
      purrr::map_chr(idx, function(i) {
        val <- table_data$revenue_30d_ww[i]
        if (is.na(val)) return("—")
        
        # Format with 2 decimals for billions
        if (val >= 1e9) {
          paste0("$", formatC(val/1e9, format = "f", digits = 2), "B")
        } else if (val >= 1e6) {
          paste0("$", round(val/1e6), "M")
        } else if (val >= 1e3) {
          paste0("$", round(val/1e3), "K")
        } else {
          paste0("$", round(val))
        }
      })
    }
  ) %>%
  
  text_transform(
    locations = cells_body(columns = revenue_alltime_ww),
    fn = function(x) {
      # Use the numeric values directly
      idx <- seq_along(x)
      purrr::map_chr(idx, function(i) {
        val <- table_data$revenue_alltime_ww[i]
        if (is.na(val)) return("—")
        
        # Format with 2 decimals for billions
        if (val >= 1e9) {
          paste0("$", formatC(val/1e9, format = "f", digits = 2), "B")
        } else if (val >= 1e6) {
          paste0("$", round(val/1e6), "M")
        } else if (val >= 1e3) {
          paste0("$", round(val/1e3), "K")
        } else {
          paste0("$", round(val))
        }
      })
    }
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
        
        # Create styled HTML with emojis and colors, with line break
        sprintf(
          '<div style="font-size: 12px; line-height: 1.3;">
            <div style="color: #4A90E2; font-weight: 500;">♂ %d%%</div>
            <div style="color: #E91E63; font-weight: 500;">♀ %d%%</div>
          </div>',
          male_pct,
          female_pct
        )
      })
    }
  ) %>%
  
  # Apply vertical heatmaps to retention columns (column-wise gradient)
  data_color(
    columns = c(retention_1d_us),
    method = "numeric",
    palette = c("#FFCDD2", "#C8E6C9", "#4CAF50"),
    domain = c(0, 100),
    na_color = "#F5F5F5"
  ) %>%
  
  data_color(
    columns = c(retention_7d_us),
    method = "numeric",
    palette = c("#FFCDD2", "#C8E6C9", "#4CAF50"),
    domain = c(0, 100),
    na_color = "#F5F5F5"
  ) %>%
  
  data_color(
    columns = c(retention_30d_us),
    method = "numeric",
    palette = c("#FFCDD2", "#C8E6C9", "#4CAF50"),
    domain = c(0, 100),
    na_color = "#F5F5F5"
  ) %>%
  
  data_color(
    columns = c(retention_60d_us),
    method = "numeric",
    palette = c("#FFCDD2", "#C8E6C9", "#4CAF50"),
    domain = c(0, 100),
    na_color = "#F5F5F5"
  ) %>%
  
  
  # Add bar charts to revenue columns (180-day and 30-day)
  text_transform(
    locations = cells_body(columns = revenue_180d_ww),
    fn = function(x) {
      # Use numeric values directly
      values <- table_data$revenue_180d_ww
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map_chr(values, function(val) {
        if (is.na(val)) return("—")
        bar_width <- (val / max_val) * 100
        
        # Format the label with 2 decimals for billions
        formatted_label <- if (val >= 1e9) {
          paste0("$", formatC(val/1e9, format = "f", digits = 2), "B")
        } else if (val >= 1e6) {
          paste0("$", round(val/1e6), "M")
        } else if (val >= 1e3) {
          paste0("$", round(val/1e3), "K")
        } else {
          paste0("$", round(val))
        }
        
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #FF6600; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          formatted_label
        )
      })
    }
  ) %>%
  
  
  # Add bar charts to revenue_30d_ww
  text_transform(
    locations = cells_body(columns = revenue_30d_ww),
    fn = function(x) {
      values <- table_data$revenue_30d_ww
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map_chr(values, function(val) {
        if (is.na(val)) return("—")
        bar_width <- (val / max_val) * 100
        
        formatted_label <- if (val >= 1e9) {
          paste0("$", formatC(val/1e9, format = "f", digits = 2), "B")
        } else if (val >= 1e6) {
          paste0("$", round(val/1e6), "M")
        } else if (val >= 1e3) {
          paste0("$", round(val/1e3), "K")
        } else {
          paste0("$", round(val))
        }
        
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #FF6600; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          formatted_label
        )
      })
    }
  ) %>%
  
  # Add bar charts to revenue_alltime_ww
  text_transform(
    locations = cells_body(columns = revenue_alltime_ww),
    fn = function(x) {
      values <- table_data$revenue_alltime_ww
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map_chr(values, function(val) {
        if (is.na(val)) return("—")
        bar_width <- (val / max_val) * 100
        
        formatted_label <- if (val >= 1e9) {
          paste0("$", formatC(val/1e9, format = "f", digits = 2), "B")
        } else if (val >= 1e6) {
          paste0("$", round(val/1e6), "M")
        } else if (val >= 1e3) {
          paste0("$", round(val/1e3), "K")
        } else {
          paste0("$", round(val))
        }
        
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #FF6600; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          formatted_label
        )
      })
    }
  ) %>%
  
  # Add bar charts with values to all downloads columns
  text_transform(
    locations = cells_body(columns = downloads_30d_ww),
    fn = function(x) {
      values <- table_data$downloads_30d_ww
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map_chr(values, function(val) {
        if (is.na(val)) return("—")
        bar_width <- (val / max_val) * 100
        
        formatted_label <- if (val >= 1e9) {
          paste0(formatC(val/1e9, format = "f", digits = 2), "B")
        } else if (val >= 1e6) {
          paste0(round(val/1e6), "M")
        } else if (val >= 1e3) {
          paste0(round(val/1e3), "K")
        } else {
          as.character(round(val))
        }
        
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #008FD5; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          formatted_label
        )
      })
    }
  ) %>%
  
  text_transform(
    locations = cells_body(columns = downloads_180d_ww),
    fn = function(x) {
      values <- table_data$downloads_180d_ww
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map_chr(values, function(val) {
        if (is.na(val)) return("—")
        bar_width <- (val / max_val) * 100
        
        formatted_label <- if (val >= 1e9) {
          paste0(formatC(val/1e9, format = "f", digits = 2), "B")
        } else if (val >= 1e6) {
          paste0(round(val/1e6), "M")
        } else if (val >= 1e3) {
          paste0(round(val/1e3), "K")
        } else {
          as.character(round(val))
        }
        
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #008FD5; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          formatted_label
        )
      })
    }
  ) %>%
  
  text_transform(
    locations = cells_body(columns = downloads_alltime_ww),
    fn = function(x) {
      values <- table_data$downloads_alltime_ww
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map_chr(values, function(val) {
        if (is.na(val)) return("—")
        bar_width <- (val / max_val) * 100
        
        formatted_label <- if (val >= 1e9) {
          paste0(formatC(val/1e9, format = "f", digits = 2), "B")
        } else if (val >= 1e6) {
          paste0(round(val/1e6), "M")
        } else if (val >= 1e3) {
          paste0(round(val/1e3), "K")
        } else {
          as.character(round(val))
        }
        
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #008FD5; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          formatted_label
        )
      })
    }
  ) %>%
  
  # Add bar charts to engagement columns (all columns)
  text_transform(
    locations = cells_body(columns = dau_30d_ww),
    fn = function(x) {
      values <- as.numeric(gsub("[^0-9.]", "", x))
      mult <- ifelse(grepl("K", x), 1000, 
              ifelse(grepl("M", x), 1000000,
              ifelse(grepl("B", x), 1000000000, 1)))
      values <- values * mult
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map2_chr(x, values, function(label, val) {
        if (is.na(val)) return("—")
        bar_width <- (val / max_val) * 100
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #9C27B0; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          label
        )
      })
    }
  ) %>%
  
  text_transform(
    locations = cells_body(columns = wau_4w_ww),
    fn = function(x) {
      values <- as.numeric(gsub("[^0-9.]", "", x))
      mult <- ifelse(grepl("K", x), 1000, 
              ifelse(grepl("M", x), 1000000,
              ifelse(grepl("B", x), 1000000000, 1)))
      values <- values * mult
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map2_chr(x, values, function(label, val) {
        if (is.na(val)) return("—")
        bar_width <- (val / max_val) * 100
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #9C27B0; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          label
        )
      })
    }
  ) %>%
  
  text_transform(
    locations = cells_body(columns = mau_month_ww),
    fn = function(x) {
      values <- as.numeric(gsub("[^0-9.]", "", x))
      mult <- ifelse(grepl("K", x), 1000, 
              ifelse(grepl("M", x), 1000000,
              ifelse(grepl("B", x), 1000000000, 1)))
      values <- values * mult
      max_val <- max(values, na.rm = TRUE)
      
      purrr::map2_chr(x, values, function(label, val) {
        if (is.na(val)) return("—")
        bar_width <- (val / max_val) * 100
        sprintf(
          '<div style="display: flex; align-items: center; gap: 8px;">
            <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
              <div style="height: 100%%; width: %s%%; background: #9C27B0; border-radius: 2px;"></div>
            </div>
            <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
          </div>',
          round(bar_width),
          label
        )
      })
    }
  ) %>%
  
  # RPD values - no bar charts, just styled numbers
  # (Formatting is already handled by fmt_currency above)
  
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
  
  # Add country flags to top revenue country column with proper HTML encoding
  text_transform(
    locations = cells_body(columns = any_of(c("most_popular_country_revenue", 
                                              "entities.custom_tags.Most Popular Country by Revenue"))),
    fn = function(x) {
      purrr::map_chr(x, function(code) {
        flag_text <- country_to_flag(code)
        # Wrap in HTML to ensure proper display
        if (flag_text != "") {
          sprintf('<span style="font-size: 14px;">%s</span>', flag_text)
        } else {
          "—"
        }
      })
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
    vwidth = 1800,
    vheight = 700
  )

cat("\nFiveThirtyEight-themed dashboard created successfully!\n")
cat("File saved to:", file.path(output_dir, "rpg_analytics_dashboard.png"), "\n")