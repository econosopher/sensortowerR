#' Create FiveThirtyEight-styled GT Dashboard from Top Charts Data
#'
#' @description
#' Creates a professional, FiveThirtyEight-themed GT table dashboard from 
#' Sensor Tower top charts data with customizable styling and metric options.
#'
#' @param data Data frame from st_top_charts() or similar Sensor Tower function
#' @param title Character string for the table title (default: "Top Mobile Games")
#' @param subtitle Character string for subtitle. If NULL, auto-generates based on data
#' @param ranking_metric Character string specifying which metric to use for ranking.
#'   Options: "revenue_180d_ww", "revenue_30d_ww", "downloads_180d_ww", "downloads_30d_ww", etc.
#'   (default: "revenue_180d_ww")
#' @param show_demographics Logical, whether to show demographic columns (age, gender)
#'   (default: TRUE)
#' @param show_engagement Logical, whether to show engagement metrics (DAU, WAU, MAU)
#'   (default: TRUE)
#' @param show_retention Logical, whether to show retention metrics
#'   (default: TRUE)
#' @param retention_region Character string for retention region ("us", "ww", etc.)
#'   (default: "us")
#' @param show_rpd Logical, whether to show Revenue Per Download
#'   (default: TRUE)
#' @param bar_charts Logical, whether to show bar chart visualizations
#'   (default: TRUE)
#' @param bar_chart_columns Character vector of column patterns to add bar charts to.
#'   If NULL, applies to all numeric columns except RPD and retention.
#' @param heatmap_retention Logical, whether to apply heatmap to retention columns
#'   (default: TRUE)
#' @param compact_mode Logical, whether to use compact row heights
#'   (default: TRUE)
#' @param width Numeric, table width in pixels (default: 1800)
#' @param height Numeric, table height in pixels (default: 700)
#' @param save_path Character string, path to save the table image. If NULL, returns GT object
#' @param icon_cache_dir Character string, directory to cache app icons
#'   (default: "inst/images/app_icons")
#' @param raw Logical, whether to return a minimally styled table without 
#'   custom formatting, bar charts, or heatmaps (default: FALSE)
#' @param color_scheme List with color codes for different metric types:
#'   - revenue: Revenue metrics color (default: "#FF6600")
#'   - downloads: Downloads metrics color (default: "#008FD5")
#'   - engagement: Engagement metrics color (default: "#9C27B0")
#'   - rpd: RPD metrics color (default: "#4CAF50")
#'   - retention_low: Low retention color (default: "#FFCDD2")
#'   - retention_mid: Mid retention color (default: "#C8E6C9")
#'   - retention_high: High retention color (default: "#4CAF50")
#'
#' @return GT object (if save_path is NULL) or saves image and returns path
#'
#' @importFrom purrr map2_chr map_chr
#' @importFrom utils data
#' @importFrom stats setNames
#' @importFrom graphics title
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage - one line after st_top_charts()
#' top_rpgs <- st_top_charts(category = 7014, measure = "revenue")
#' st_gt_dashboard(top_rpgs)
#' 
#' # Raw mode for minimal styling
#' st_gt_dashboard(top_rpgs, raw = TRUE)
#' 
#' # Customize the dashboard
#' st_gt_dashboard(
#'   top_rpgs,
#'   title = "Top Role-Playing Games Q4 2024",
#'   ranking_metric = "revenue_30d_ww",
#'   show_retention = FALSE,
#'   save_path = "dashboard.png"
#' )
#' 
#' # Change color scheme
#' st_gt_dashboard(
#'   top_rpgs,
#'   color_scheme = list(
#'     revenue = "#E74C3C",
#'     downloads = "#3498DB",
#'     engagement = "#9B59B6"
#'   )
#' )
#' }
st_gt_dashboard <- function(
  data,
  title = "Top Mobile Games",
  subtitle = NULL,
  ranking_metric = "revenue_180d_ww",
  show_demographics = TRUE,
  show_engagement = TRUE,
  show_retention = TRUE,
  retention_region = "us",
  show_rpd = TRUE,
  bar_charts = TRUE,
  bar_chart_columns = NULL,
  heatmap_retention = TRUE,
  compact_mode = TRUE,
  width = 1800,
  height = 700,
  save_path = NULL,
  icon_cache_dir = "inst/images/app_icons",
  raw = FALSE,
  color_scheme = list(
    revenue = "#FF6600",
    downloads = "#008FD5", 
    engagement = "#9C27B0",
    rpd = "#4CAF50",
    retention_low = "#FFCDD2",
    retention_mid = "#C8E6C9",
    retention_high = "#4CAF50"
  )
) {
  
  # Check for required packages
  if (!requireNamespace("gt", quietly = TRUE)) {
    rlang::abort("Package 'gt' is required for this function. Please install it with: install.packages('gt')")
  }
  if (!requireNamespace("gtExtras", quietly = TRUE)) {
    rlang::abort("Package 'gtExtras' is required for this function. Please install it with: install.packages('gtExtras')")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    rlang::abort("Package 'stringr' is required for this function. Please install it with: install.packages('stringr')")
  }
  
  # Override styling options if raw mode
  if (raw) {
    bar_charts <- FALSE
    heatmap_retention <- FALSE
  }
  
  # Helper function to format billions with 2 decimals
  format_billions <- function(val) {
    if (is.na(val)) return("-")
    if (val >= 1e9) {
      paste0(formatC(val/1e9, format = "f", digits = 2), "B")
    } else if (val >= 1e6) {
      paste0(round(val/1e6), "M")
    } else if (val >= 1e3) {
      paste0(round(val/1e3), "K")
    } else {
      as.character(round(val))
    }
  }
  
  # Helper function to format currency with billions
  format_currency_billions <- function(val) {
    if (is.na(val)) return("-")
    if (val >= 1e9) {
      paste0("$", formatC(val/1e9, format = "f", digits = 2), "B")
    } else if (val >= 1e6) {
      paste0("$", round(val/1e6), "M")
    } else if (val >= 1e3) {
      paste0("$", round(val/1e3), "K")
    } else {
      paste0("$", round(val))
    }
  }
  
  # Helper function to create bar chart HTML
  create_bar_chart <- function(values, labels, color, format_fn = NULL, use_labels = FALSE) {
    max_val <- max(values, na.rm = TRUE)
    
    purrr::map2_chr(values, labels, function(val, label) {
      if (is.na(val)) return("-")
      bar_width <- (val / max_val) * 100
      
      # Use provided labels or format the value
      if (use_labels) {
        formatted_label <- label
      } else if (!is.null(format_fn)) {
        formatted_label <- format_fn(val)
      } else {
        formatted_label <- as.character(val)
      }
      
      sprintf(
        '<div style="display: flex; align-items: center; gap: 8px;">
          <div style="flex: 0 0 60px; height: 15px; background: #E1E1E1; position: relative; border-radius: 2px;">
            <div style="height: 100%%; width: %s%%; background: %s; border-radius: 2px;"></div>
          </div>
          <span style="font-size: 12px; color: #333; font-weight: 500;">%s</span>
        </div>',
        round(bar_width),
        color,
        formatted_label
      )
    })
  }
  
  # Country to flag emoji mapping
  country_to_flag <- function(country_code) {
    if (is.na(country_code) || country_code == "") return("")
    
    country_code <- toupper(country_code)
    
    # Using ASCII-only country codes instead of flag emojis
    flags <- c(
      "US" = "[US]", "GB" = "[GB]", "CA" = "[CA]", "AU" = "[AU]", "DE" = "[DE]",
      "FR" = "[FR]", "IT" = "[IT]", "ES" = "[ES]", "NL" = "[NL]", "SE" = "[SE]",
      "NO" = "[NO]", "DK" = "[DK]", "FI" = "[FI]", "PL" = "[PL]", "RU" = "[RU]",
      "BR" = "[BR]", "MX" = "[MX]", "AR" = "[AR]", "CL" = "[CL]", "CO" = "[CO]",
      "JP" = "[JP]", "KR" = "[KR]", "CN" = "[CN]", "TW" = "[TW]", "HK" = "[HK]",
      "SG" = "[SG]", "MY" = "[MY]", "TH" = "[TH]", "ID" = "[ID]", "PH" = "[PH]",
      "VN" = "[VN]", "IN" = "[IN]", "PK" = "[PK]", "BD" = "[BD]", "LK" = "[LK]",
      "AE" = "[AE]", "SA" = "[SA]", "EG" = "[EG]", "IL" = "[IL]", "TR" = "[TR]",
      "ZA" = "[ZA]", "NG" = "[NG]", "KE" = "[KE]", "GH" = "[GH]", "MA" = "[MA]",
      "PT" = "[PT]", "GR" = "[GR]", "CH" = "[CH]", "AT" = "[AT]", "BE" = "[BE]",
      "CZ" = "[CZ]", "HU" = "[HU]", "RO" = "[RO]", "BG" = "[BG]", "SK" = "[SK]",
      "HR" = "[HR]", "SI" = "[SI]", "LT" = "[LT]", "LV" = "[LV]", "EE" = "[EE]",
      "IE" = "[IE]", "NZ" = "[NZ]", "PE" = "[PE]", "VE" = "[VE]", "EC" = "[EC]",
      "UY" = "[UY]", "PY" = "[PY]", "BO" = "[BO]", "CR" = "[CR]", "PA" = "[PA]",
      "GT" = "[GT]", "HN" = "[HN]", "SV" = "[SV]", "NI" = "[NI]", "DO" = "[DO]",
      "JM" = "[JM]", "TT" = "[TT]", "CU" = "[CU]", "PR" = "[PR]", "LU" = "[LU]",
      "IS" = "[IS]", "MT" = "[MT]", "CY" = "[CY]", "AL" = "[AL]", "MK" = "[MK]",
      "RS" = "[RS]", "BA" = "[BA]", "ME" = "[ME]", "XK" = "[XK]", "MD" = "[MD]",
      "UA" = "[UA]", "BY" = "[BY]", "AM" = "[AM]", "GE" = "[GE]", "AZ" = "[AZ]",
      "KZ" = "[KZ]", "UZ" = "[UZ]", "TM" = "[TM]", "KG" = "[KG]", "TJ" = "[TJ]"
    )
    
    if (country_code %in% names(flags)) {
      return(paste0(flags[country_code], " ", country_code))
    } else {
      return(country_code)
    }
  }
  
  # Prepare data
  table_data <- data %>%
    arrange(desc(!!sym(ranking_metric))) %>%
    mutate(
      rank = row_number(),
      # Map gender column if it exists
      genders_ww = if ("entities.custom_tags.Genders (Last Quarter, WW)" %in% names(.)) {
        `entities.custom_tags.Genders (Last Quarter, WW)`
      } else {
        NA_character_
      },
      # Calculate years since launch
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
        TRUE ~ "-"
      )
    )
  
  # Build column selection dynamically
  selected_cols <- c("rank", "unified_app_name")
  
  # Add ranking metric
  selected_cols <- c(selected_cols, ranking_metric)
  
  # Add demographics if requested
  if (show_demographics) {
    demo_cols <- c("age_ww", "genders_ww", "years_since_launch")
    
    # Check for country column variants
    country_col <- if ("entities.custom_tags.Most Popular Country by Revenue" %in% names(table_data)) {
      "entities.custom_tags.Most Popular Country by Revenue"
    } else if ("most_popular_country_revenue" %in% names(table_data)) {
      "most_popular_country_revenue"
    } else {
      NULL
    }
    
    if (!is.null(country_col)) demo_cols <- c(demo_cols, country_col)
    
    selected_cols <- c(selected_cols, intersect(names(table_data), demo_cols))
  }
  
  # Add revenue columns
  revenue_cols <- c("revenue_30d_ww", "revenue_alltime_ww")
  if (show_rpd) revenue_cols <- c(revenue_cols, "rpd_alltime_ww")
  selected_cols <- c(selected_cols, intersect(names(table_data), revenue_cols))
  
  # Add engagement if requested
  if (show_engagement) {
    engagement_cols <- c("dau_30d_ww", "wau_4w_ww", "mau_month_ww")
    selected_cols <- c(selected_cols, intersect(names(table_data), engagement_cols))
  }
  
  # Add downloads
  downloads_cols <- c("downloads_30d_ww", "downloads_180d_ww", "downloads_alltime_ww")
  selected_cols <- c(selected_cols, intersect(names(table_data), downloads_cols))
  
  # Add retention if requested
  available_retention <- character(0)  # Initialize for later use
  if (show_retention) {
    retention_cols <- paste0("retention_", c("1d", "7d", "30d", "60d"), "_", retention_region)
    available_retention <- intersect(names(table_data), retention_cols)
    selected_cols <- c(selected_cols, available_retention)
    
    # Retention values should already be in percentage format (0-100)
    # No conversion needed
  }
  
  # Select final columns
  table_data <- table_data %>%
    select(all_of(selected_cols))
  
  # Auto-generate subtitle if not provided
  if (is.null(subtitle)) {
    date_range <- paste0(
      format(Sys.Date() - 180, "%b %Y"), "-",
      format(Sys.Date(), "%b %Y")
    )
    
    ranking_label <- case_when(
      grepl("revenue_180d", ranking_metric) ~ "180-day revenue",
      grepl("revenue_30d", ranking_metric) ~ "30-day revenue", 
      grepl("revenue_alltime", ranking_metric) ~ "lifetime revenue",
      grepl("downloads_180d", ranking_metric) ~ "180-day downloads",
      grepl("downloads_30d", ranking_metric) ~ "30-day downloads",
      grepl("downloads_alltime", ranking_metric) ~ "lifetime downloads",
      TRUE ~ ranking_metric
    )
    
    subtitle <- paste0("Ranked by ", ranking_label, " (", date_range, ")")
  }
  
  # Create GT table
  gt_table <- table_data %>%
    gt::gt()
  
  # Apply theme based on raw parameter
  if (!raw) {
    gt_table <- gt_table %>%
      gtExtras::gt_theme_538()
  }
  
  # Add header
  gt_table <- gt_table %>%
    gt::tab_header(
      title = title,
      subtitle = subtitle
    )
  
  # Apply compact mode if requested
  if (compact_mode && !raw) {
    gt_table <- gt_table %>%
      gt::tab_options(
        data_row.padding = gt::px(2),
        row.striping.include_table_body = FALSE
      )
  }
  
  # Center all column headers
  gt_table <- gt_table %>%
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_column_labels(everything())
    )
  
  # Build column labels dynamically
  col_labels <- list(
    rank = "",
    unified_app_name = "Game",
    age_ww = "Avg Age",
    genders_ww = "Gender",
    years_since_launch = "Years Live",
    revenue_30d_ww = "30-Day Revenue",
    revenue_alltime_ww = "Lifetime Revenue", 
    rpd_alltime_ww = "Revenue per Download",
    downloads_30d_ww = "30-Day Downloads",
    downloads_180d_ww = "180-Day Downloads",
    downloads_alltime_ww = "Lifetime Downloads",
    dau_30d_ww = "DAU (30d avg)",
    wau_4w_ww = "WAU (4w avg)",
    mau_month_ww = "MAU (monthly avg)"
  )
  
  # Add retention labels
  if (show_retention) {
    retention_labels <- list(
      "retention_1d" = "Day 1",
      "retention_7d" = "Day 7",
      "retention_30d" = "Day 30",
      "retention_60d" = "Day 60"
    )
    
    for (ret_type in names(retention_labels)) {
      col_name <- paste0(ret_type, "_", retention_region)
      if (col_name %in% names(table_data)) {
        col_labels[[col_name]] <- retention_labels[[ret_type]]
      }
    }
  }
  
  # Add ranking metric label
  col_labels[[ranking_metric]] <- if (raw) {
    "Ranking Metric"
  } else {
    paste0(gsub("_", " ", gsub("_ww$", "", ranking_metric)), " (Ranking KPI)")
  }
  
  # Add country column label
  if ("entities.custom_tags.Most Popular Country by Revenue" %in% names(table_data)) {
    col_labels[["entities.custom_tags.Most Popular Country by Revenue"]] <- "Top Country (Rev)"
  }
  if ("most_popular_country_revenue" %in% names(table_data)) {
    col_labels[["most_popular_country_revenue"]] <- "Top Country (Rev)"
  }
  
  # Filter col_labels to only include columns that exist in the table
  existing_cols <- names(table_data)
  col_labels <- col_labels[names(col_labels) %in% existing_cols]
  
  # Apply column labels
  gt_table <- gt_table %>%
    gt::cols_label(!!!col_labels)
  
  # Add column groups (spanners)
  if (!raw || ranking_metric %in% names(table_data)) {
    gt_table <- gt_table %>%
      gt::tab_spanner(
        label = "RANKING KPI",
        columns = all_of(ranking_metric)
      )
  }
  
  if (show_demographics && any(c("age_ww", "genders_ww", "years_since_launch") %in% names(table_data))) {
    demo_cols_present <- intersect(names(table_data), 
                                   c("age_ww", "genders_ww", "years_since_launch",
                                     "entities.custom_tags.Most Popular Country by Revenue",
                                     "most_popular_country_revenue"))
    gt_table <- gt_table %>%
      gt::tab_spanner(
        label = "DEMOGRAPHICS", 
        columns = all_of(demo_cols_present)
      )
  }
  
  revenue_cols_present <- intersect(names(table_data), 
                                    c("revenue_30d_ww", "revenue_alltime_ww", "rpd_alltime_ww"))
  if (length(revenue_cols_present) > 0) {
    gt_table <- gt_table %>%
      gt::tab_spanner(
        label = "REVENUE",
        columns = all_of(revenue_cols_present)
      )
  }
  
  if (show_engagement) {
    engagement_cols_present <- intersect(names(table_data),
                                         c("dau_30d_ww", "wau_4w_ww", "mau_month_ww"))
    if (length(engagement_cols_present) > 0) {
      gt_table <- gt_table %>%
        gt::tab_spanner(
          label = "ENGAGEMENT",
          columns = all_of(engagement_cols_present)
        )
    }
  }
  
  downloads_cols_present <- intersect(names(table_data),
                                      c("downloads_30d_ww", "downloads_180d_ww", "downloads_alltime_ww"))
  if (length(downloads_cols_present) > 0) {
    gt_table <- gt_table %>%
      gt::tab_spanner(
        label = "DOWNLOADS",
        columns = all_of(downloads_cols_present)
      )
  }
  
  if (show_retention && length(available_retention) > 0) {
    gt_table <- gt_table %>%
      gt::tab_spanner(
        label = paste0("RETENTION (", toupper(retention_region), ")"),
        columns = all_of(available_retention)
      )
  }
  
  # Apply number formatting
  gt_table <- gt_table %>%
    gt::fmt_number(
      columns = rank,
      decimals = 0
    ) %>%
    gt::sub_missing(
      columns = everything(),
      missing_text = "-"
    )
  
  # Format numeric columns based on raw parameter
  if (raw) {
    # Simple formatting for raw mode
    gt_table <- gt_table %>%
      gt::fmt_number(
        columns = any_of(c("age_ww", "dau_30d_ww", "wau_4w_ww", "mau_month_ww",
                          "downloads_30d_ww", "downloads_180d_ww", "downloads_alltime_ww")),
        decimals = 0,
        suffixing = TRUE
      ) %>%
      gt::fmt_currency(
        columns = any_of(c(ranking_metric, "revenue_30d_ww", "revenue_alltime_ww")),
        currency = "USD",
        decimals = 0,
        suffixing = TRUE
      ) %>%
      gt::fmt_currency(
        columns = any_of("rpd_alltime_ww"),
        currency = "USD",
        decimals = 2
      ) %>%
      gt::fmt_percent(
        columns = any_of(available_retention),
        decimals = 0,
        scale_values = FALSE
      )
  } else {
    # Full formatting with bar charts and custom styling
    
    # Format engagement metrics
    gt_table <- gt_table %>%
      gt::fmt_number(
        columns = any_of(c("age_ww", "dau_30d_ww", "wau_4w_ww", "mau_month_ww")),
        decimals = 0,
        suffixing = TRUE
      ) %>%
      gt::fmt_currency(
        columns = any_of("rpd_alltime_ww"),
        currency = "USD",
        decimals = 2
      ) %>%
      gt::fmt_percent(
        columns = any_of(available_retention),
        decimals = 0,
        scale_values = FALSE
      )
    
    # Apply bar charts if requested
    if (bar_charts) {
      # Revenue columns with bar charts
      if (ranking_metric %in% names(table_data)) {
        # Get original numeric values
        original_ranking_values <- table_data[[ranking_metric]]
        
        gt_table <- gt_table %>%
          gt::text_transform(
            locations = gt::cells_body(columns = all_of(ranking_metric)),
            fn = function(x) {
              create_bar_chart(
                original_ranking_values, 
                x, 
                color_scheme$revenue,
                format_currency_billions
              )
            }
          )
      }
      
      if ("revenue_30d_ww" %in% names(table_data)) {
        gt_table <- gt_table %>%
          gt::text_transform(
            locations = gt::cells_body(columns = revenue_30d_ww),
            fn = function(x) {
              create_bar_chart(
                table_data$revenue_30d_ww,
                x,
                color_scheme$revenue,
                format_currency_billions
              )
            }
          )
      }
      
      if ("revenue_alltime_ww" %in% names(table_data)) {
        gt_table <- gt_table %>%
          gt::text_transform(
            locations = gt::cells_body(columns = revenue_alltime_ww),
            fn = function(x) {
              create_bar_chart(
                table_data$revenue_alltime_ww,
                x,
                color_scheme$revenue,
                format_currency_billions
              )
            }
          )
      }
      
      # Downloads columns with bar charts
      for (dl_col in c("downloads_30d_ww", "downloads_180d_ww", "downloads_alltime_ww")) {
        if (dl_col %in% names(table_data)) {
          gt_table <- gt_table %>%
            gt::text_transform(
              locations = gt::cells_body(columns = all_of(dl_col)),
              fn = function(x) {
                create_bar_chart(
                  table_data[[dl_col]],
                  x,
                  color_scheme$downloads,
                  format_billions
                )
              }
            )
        }
      }
      
      # Engagement columns with bar charts
      for (eng_col in c("dau_30d_ww", "wau_4w_ww", "mau_month_ww")) {
        if (eng_col %in% names(table_data)) {
          # Get the original numeric values before formatting
          original_values <- table_data[[eng_col]]
          
          gt_table <- gt_table %>%
            gt::text_transform(
              locations = gt::cells_body(columns = all_of(eng_col)),
              fn = function(x) {
                # x contains the formatted values (e.g., "779M", "2.1B")
                # Use the formatted values as labels
                create_bar_chart(
                  original_values, 
                  x,  # Use the pre-formatted values as labels
                  color_scheme$engagement,
                  use_labels = TRUE  # Use the labels from x
                )
              }
            )
        }
      }
    }
    
    # Apply retention heatmaps if requested
    if (heatmap_retention && show_retention && length(available_retention) > 0) {
      for (ret_col in available_retention) {
        gt_table <- gt_table %>%
          gt::data_color(
            columns = all_of(ret_col),
            method = "numeric",
            palette = c(color_scheme$retention_low, 
                       color_scheme$retention_mid,
                       color_scheme$retention_high),
            domain = c(0, 100),
            na_color = "#F5F5F5"
          )
      }
    }
    
    # Format gender column with custom HTML
    if ("genders_ww" %in% names(table_data)) {
      gt_table <- gt_table %>%
        text_transform(
          locations = cells_body(columns = genders_ww),
          fn = function(x) {
            purrr::map_chr(x, function(gender) {
              if (is.na(gender) || gender == "") return("-")
              
              # Parse the gender percentage
              male_match <- regexpr("[0-9]+(?=% Male)", gender, perl = TRUE)
              if (male_match > 0) {
                male_pct <- as.numeric(regmatches(gender, male_match))
              } else {
                male_pct <- as.numeric(gsub("[^0-9]", "", gender))
              }
              
              female_pct <- 100 - male_pct
              
              sprintf(
                '<div style="font-size: 12px; line-height: 1.3;">
                  <div style="color: #4A90E2; font-weight: 500;">\u2642 %d%%</div>
                  <div style="color: #E91E63; font-weight: 500;">\u2640 %d%%</div>
                </div>',
                male_pct,
                female_pct
              )
            })
          }
        )
    }
    
    # Add country flags
    country_cols <- c("entities.custom_tags.Most Popular Country by Revenue", 
                      "most_popular_country_revenue")
    country_col <- intersect(names(table_data), country_cols)
    
    if (length(country_col) > 0) {
      gt_table <- gt_table %>%
        text_transform(
          locations = cells_body(columns = all_of(country_col)),
          fn = function(x) {
            purrr::map_chr(x, function(code) {
              flag_text <- country_to_flag(code)
              if (flag_text != "") {
                sprintf('<span style="font-size: 14px;">%s</span>', flag_text)
              } else {
                "-"
              }
            })
          }
        )
    }
    
    # Style the rank column
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          cell_text(
            size = px(20),
            weight = "bold",
            color = "#666666"
          )
        ),
        locations = cells_body(columns = rank)
      )
    
    # Add medal emojis to top 3
    gt_table <- gt_table %>%
      text_transform(
        locations = cells_body(columns = rank, rows = 1),
        fn = function(x) paste0(x, " \U0001f947")
      ) %>%
      text_transform(
        locations = cells_body(columns = rank, rows = 2),
        fn = function(x) paste0(x, " \U0001f948")
      ) %>%
      text_transform(
        locations = cells_body(columns = rank, rows = 3),
        fn = function(x) paste0(x, " \U0001f949")
      )
    
    # Highlight top 3 games
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#F2F2F2"),
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          rows = 1:3,
          columns = unified_app_name
        )
      )
    
    # Style game names
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          cell_text(
            size = px(14),
            weight = "600"
          )
        ),
        locations = cells_body(columns = unified_app_name)
      )
  }
  
  # Add footnotes
  if (!raw) {
    gt_table <- gt_table %>%
      gt::tab_footnote(
        footnote = "Data represents worldwide metrics unless otherwise specified",
        locations = gt::cells_column_labels(columns = all_of(ranking_metric))
      )
    
    # Check if we have engagement columns to add footnote
    engagement_cols_in_table <- intersect(names(table_data), 
                                         c("dau_30d_ww", "wau_4w_ww", "mau_month_ww"))
    if (show_engagement && length(engagement_cols_in_table) > 0) {
      gt_table <- gt_table %>%
        gt::tab_footnote(
          footnote = "All engagement metrics show average values over their respective time periods",
          locations = gt::cells_column_spanners(spanners = "ENGAGEMENT")
        )
    }
    
    gt_table <- gt_table %>%
      gt::tab_source_note(
        source_note = gt::md("**Source:** Sensor Tower Store Intelligence")
      )
  }
  
  # Save or return
  if (!is.null(save_path)) {
    gt_table %>%
      gt::gtsave(
        filename = save_path,
        vwidth = width,
        vheight = height
      )
    message("Dashboard saved to: ", save_path)
    return(invisible(save_path))
  } else {
    return(gt_table)
  }
}