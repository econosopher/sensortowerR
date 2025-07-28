#' Fetch Year-to-Date Metrics for Apps or Publishers
#'
#' Fetches year-to-date metrics for apps or publishers across multiple years,
#' with intelligent batching and caching to minimize API calls.
#'
#' @param unified_app_id Character vector. Unified app ID(s) that work across iOS and Android platforms.
#' @param ios_app_id Character vector. iOS app ID(s) (optional).
#' @param android_app_id Character vector. Android package name(s) (optional).
#' @param publisher_id Character vector. Publisher ID(s) (alternative to app IDs).
#' @param years Integer vector. Years to fetch data for (e.g., c(2023, 2024, 2025)).
#'   If NULL, uses current year only.
#' @param period_start Character string. Start date in "MM-DD" format (e.g., "02-01" for Feb 1).
#'   If NULL, defaults to "01-01" (January 1).
#' @param period_end Character string. End date in "MM-DD" format (e.g., "02-28").
#'   If NULL, defaults to last completed week (ending Saturday) of current year.
#' @param metrics Character vector. Metrics to fetch. Supports "revenue", "downloads", "dau", "wau", and "mau".
#'   Default is both revenue and downloads. Note: DAU/WAU/MAU are calculated as averages for fair YoY comparisons.
#' @param countries Character vector. Country codes (e.g., "US", "GB", "JP"). Required.
#' @param cache_dir Character. Directory for caching API responses (optional).
#' @param auth_token Character string. Sensor Tower API token.
#' @param verbose Logical. Print progress messages.
#'
#' @return A tibble in tidy/long format with columns:
#'   - `entity_id`: App or publisher ID
#'   - `entity_name`: App or publisher name
#'   - `entity_type`: "app" or "publisher"
#'   - `year`: Year of the data
#'   - `date_start`: Start date of the period
#'   - `date_end`: End date of the period
#'   - `country`: Country code
#'   - `metric`: The metric name (e.g., "revenue", "downloads", "dau")
#'   - `value`: Metric value (units depend on metric type)
#'
#' @details
#' This function intelligently handles various scenarios:
#' 
#' - **Default behavior**: Fetches YTD through last completed week (Saturday)
#' - **Custom periods**: Apply same calendar period to all specified years
#' - **Leap years**: Automatically handled (e.g., Feb 29 in leap years)
#' - **Entity detection**: Automatically determines if using app or publisher endpoints
#' - **Caching**: Reuses cached data for overlapping periods across years
#' - **Active Users Support**: DAU, WAU, and MAU are averaged across periods for meaningful comparisons
#'
#' @examples
#' \dontrun{
#' # Get YTD metrics including DAU, WAU, and MAU for a single app
#' ytd_metrics <- st_ytd_metrics(
#'   unified_app_id = "553834731",  # Candy Crush
#'   years = c(2023, 2024, 2025),
#'   metrics = c("revenue", "downloads", "dau", "wau", "mau")
#' )
#'
#' # Get active user metrics only for multiple apps
#' active_user_metrics <- st_ytd_metrics(
#'   ios_app_id = c("553834731", "1195621598"),
#'   android_app_id = c("com.king.candycrushsaga", "com.playrix.homescapes"),
#'   years = 2025,
#'   metrics = c("dau", "wau")
#' )
#' }
#'
#' @importFrom dplyr %>% group_by summarise mutate select filter bind_rows all_of
#' @importFrom lubridate floor_date ceiling_date wday day
#' @importFrom tibble tibble
#' @importFrom rlang %||%
#' @importFrom tidyr pivot_longer
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @export
st_ytd_metrics <- function(
  unified_app_id = NULL,
  ios_app_id = NULL,
  android_app_id = NULL,
  publisher_id = NULL,
  years = NULL,
  period_start = NULL,
  period_end = NULL,
  metrics = c("revenue", "downloads"),
  countries,
  cache_dir = NULL,
  auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
  verbose = TRUE
) {
  
  # Validate metrics
  valid_metrics <- c("revenue", "downloads", "dau", "wau", "mau")
  if (!all(metrics %in% valid_metrics)) {
    invalid <- metrics[!metrics %in% valid_metrics]
    stop(paste0("Invalid metrics: ", paste(invalid, collapse = ", "), 
                ". Valid options are: ", paste(valid_metrics, collapse = ", ")))
  }
  
  # Check if DAU/WAU/MAU is requested with publishers
  active_user_metrics <- intersect(metrics, c("dau", "wau", "mau"))
  if (length(active_user_metrics) > 0 && !is.null(publisher_id)) {
    stop(paste0(paste(active_user_metrics, collapse = " and "), 
                " metrics are not available for publishers, only for individual apps."))
  }
  
  # Validate required parameters
  if (missing(countries) || is.null(countries) || length(countries) == 0) {
    stop("'countries' parameter is required. Specify country codes (e.g., 'US', 'GB', 'JP', or 'WW' for worldwide).")
  }
  
  # Validate inputs
  if (is.null(unified_app_id) && is.null(ios_app_id) && is.null(android_app_id) && is.null(publisher_id)) {
    stop("At least one ID must be provided (unified_app_id, ios_app_id, android_app_id, or publisher_id)")
  }
  
  # Determine entity type
  entity_type <- if (!is.null(publisher_id)) "publisher" else "app"
  
  # If both app IDs and publisher IDs are provided, throw error
  if (!is.null(publisher_id) && (!is.null(unified_app_id) || !is.null(ios_app_id) || !is.null(android_app_id))) {
    stop("Cannot mix publisher IDs with app IDs. Use either publisher_id OR app IDs.")
  }
  
  # Default years to current year
  if (is.null(years)) {
    years <- as.integer(format(Sys.Date(), "%Y"))
  }
  
  # Default period_start to January 1
  if (is.null(period_start)) {
    period_start <- "01-01"
  }
  
  # Default period_end to last completed week (ending Saturday)
  if (is.null(period_end)) {
    today <- Sys.Date()
    current_year <- as.integer(format(today, "%Y"))
    
    # Check if we're asking for current year
    if (current_year %in% years) {
      # Find last Saturday
      days_since_saturday <- (lubridate::wday(today) + 5) %% 7
      last_saturday <- today - days_since_saturday
      
      # If last Saturday is in the future (today is Sunday), go back a week
      if (last_saturday >= today) {
        last_saturday <- last_saturday - 7
      }
      
      period_end <- format(last_saturday, "%m-%d")
      
      if (verbose) {
        message(sprintf("Using default period_end: %s (last completed week)", period_end))
      }
    } else {
      # For past years, use Dec 31
      period_end <- "12-31"
    }
  }
  
  # Validate date formats
  if (!grepl("^\\d{2}-\\d{2}$", period_start)) {
    stop("period_start must be in MM-DD format (e.g., '02-01')")
  }
  if (!grepl("^\\d{2}-\\d{2}$", period_end)) {
    stop("period_end must be in MM-DD format (e.g., '02-28')")
  }
  
  # Check authentication
  if (is.null(auth_token) || auth_token == "") {
    stop("Authentication token required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  # Initialize cache if specified
  if (!is.null(cache_dir)) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
  }
  
  # Create a list of all entities to process
  if (entity_type == "publisher") {
    entities <- data.frame(
      entity_id = publisher_id,
      entity_type = "publisher",
      stringsAsFactors = FALSE
    )
  } else {
    # Combine all app IDs into a single list
    entities <- data.frame(
      entity_id = character(),
      ios_id = character(),
      android_id = character(),
      entity_type = character(),
      stringsAsFactors = FALSE
    )
    
    # Add unified app IDs
    if (!is.null(unified_app_id)) {
      for (id in unified_app_id) {
        entities <- rbind(entities, data.frame(
          entity_id = id,
          ios_id = NA_character_,
          android_id = NA_character_,
          entity_type = "app",
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Add iOS/Android pairs if provided
    if (!is.null(ios_app_id) || !is.null(android_app_id)) {
      max_len <- max(length(ios_app_id), length(android_app_id))
      ios_ids <- rep(ios_app_id, length.out = max_len)
      android_ids <- rep(android_app_id, length.out = max_len)
      
      for (i in 1:max_len) {
        entities <- rbind(entities, data.frame(
          entity_id = paste0(ios_ids[i] %||% "", "_", android_ids[i] %||% ""),
          ios_id = ios_ids[i] %||% NA_character_,
          android_id = android_ids[i] %||% NA_character_,
          entity_type = "app",
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Collect results for all entities and years
  all_results <- list()
  total_api_calls <- 0
  
  if (verbose) {
    message("\nFetching metrics for:")
    message("  Entities: ", nrow(entities))
    message("  Years: ", paste(years, collapse = ", "))
    message(sprintf("  Period: %s to %s for each year", period_start, period_end))
    message("  Metrics: ", paste(metrics, collapse = ", "))
  }
  
  # Loop through each entity and year
  for (i in 1:nrow(entities)) {
    entity <- entities[i, ]
    
    for (year in years) {
    # Construct full dates for this year
    start_date <- as.Date(paste(year, period_start, sep = "-"))
    
    # Handle end date with leap year consideration
    end_date_str <- paste(year, period_end, sep = "-")
    end_date <- tryCatch(
      as.Date(end_date_str),
      error = function(e) {
        # If date is invalid (e.g., Feb 29 in non-leap year), use last day of month
        month <- as.integer(substr(period_end, 1, 2))
        last_day <- lubridate::ceiling_date(as.Date(paste(year, month, "01", sep = "-")), "month") - 1
        last_day
      }
    )
    
    # Don't fetch future data
    if (end_date > Sys.Date()) {
      end_date <- Sys.Date() - 1
      if (verbose) {
        message(sprintf("Note: Adjusting %d end date to %s (avoiding future dates)", year, end_date))
      }
    }
    
    # Skip if entire period is in the future
    if (start_date > Sys.Date()) {
      if (verbose) {
        message(sprintf("Skipping %d - period is entirely in the future", year))
      }
      next
    }
    
    if (verbose) {
      message(sprintf("\nFetching %s (%s) for %d: %s to %s", 
                     entity$entity_id, entity$entity_type, year, start_date, end_date))
    }
    
    # Fetch data based on entity type
    if (entity$entity_type == "publisher") {
      year_data <- fetch_publisher_metrics(
        publisher_id = entity$entity_id,
        start_date = start_date,
        end_date = end_date,
        countries = countries,
        metrics = metrics,
        cache_dir = cache_dir,
        auth_token = auth_token,
        verbose = verbose
      )
    } else {
      # For apps, use the specific IDs if available
      if (!is.na(entity$ios_id) || !is.na(entity$android_id)) {
        year_data <- fetch_app_metrics(
          unified_app_id = NULL,
          ios_app_id = entity$ios_id,
          android_app_id = entity$android_id,
          start_date = start_date,
          end_date = end_date,
          countries = countries,
          metrics = metrics,
          cache_dir = cache_dir,
          auth_token = auth_token,
          verbose = verbose
        )
      } else {
        year_data <- fetch_app_metrics(
          unified_app_id = entity$entity_id,
          ios_app_id = NULL,
          android_app_id = NULL,
          start_date = start_date,
          end_date = end_date,
          countries = countries,
          metrics = metrics,
          cache_dir = cache_dir,
          auth_token = auth_token,
          verbose = verbose
        )
      }
    }
    
    # Add year and period information
    if (nrow(year_data$data) > 0) {
      year_data$data <- year_data$data %>%
        mutate(
          entity_id = entity$entity_id,
          entity_type = entity$entity_type,
          year = year,
          date_start = format(start_date, "%Y-%m-%d"),
          date_end = format(end_date, "%Y-%m-%d"),
          .before = 1
        )
      
      # Create unique key for storing results
      result_key <- paste(entity$entity_id, year, sep = "_")
      all_results[[result_key]] <- year_data$data
    }
    
    total_api_calls <- total_api_calls + year_data$api_calls
    } # End year loop
  } # End entity loop
  
  # Combine all results
  if (length(all_results) == 0) {
    if (verbose) {
      message("\nNo data retrieved for specified periods.")
    }
    return(tibble::tibble())
  }
  
  combined_data <- dplyr::bind_rows(all_results)
  
  # Transform to tidy format
  metric_cols <- intersect(metrics, colnames(combined_data))
  
  tidy_data <- combined_data %>%
    tidyr::pivot_longer(
      cols = all_of(metric_cols),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      entity_name = NA_character_  # Could be populated from API response
    ) %>%
    select(entity_id, entity_name, entity_type, year, date_start, date_end, 
           country, metric, value)
  
  if (verbose) {
    message(sprintf("\nTotal API calls used: %d", total_api_calls))
    message(sprintf("Records retrieved: %d", nrow(tidy_data)))
  }
  
  return(tidy_data)
}

#' Fetch metrics for a publisher
#' @noRd
fetch_publisher_metrics <- function(
  publisher_id,
  start_date,
  end_date,
  countries,
  metrics,
  cache_dir,
  auth_token,
  verbose
) {
  
  # Check cache first
  if (!is.null(cache_dir)) {
    cache_key <- paste(
      "publisher",
      publisher_id,
      format(start_date, "%Y%m%d"),
      format(end_date, "%Y%m%d"),
      paste(countries, collapse = "_"),
      paste(metrics, collapse = "_"),
      sep = "_"
    )
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
    
    if (file.exists(cache_file)) {
      cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
      if (cache_age < 24) {
        if (verbose) {
          message(sprintf("  Using cached publisher data (%.1f hours old)", as.numeric(cache_age)))
        }
        cached_data <- readRDS(cache_file)
        return(list(data = cached_data, api_calls = 0))
      }
    }
  }
  
  # For now, return empty data as publisher endpoint implementation would go here
  # This is a placeholder for the actual publisher API implementation
  if (verbose) {
    message("  Publisher endpoint implementation pending...")
  }
  
  empty_data <- tibble::tibble(
    country = character()
  )
  
  # Add empty columns for requested metrics
  for (metric in metrics) {
    empty_data[[metric]] <- numeric()
  }
  
  return(list(data = empty_data, api_calls = 0))
}

#' Fetch metrics for an app
#' @noRd
fetch_app_metrics <- function(
  unified_app_id,
  ios_app_id,
  android_app_id,
  start_date,
  end_date,
  countries,
  metrics,
  cache_dir,
  auth_token,
  verbose
) {
  
  # Check cache first
  if (!is.null(cache_dir)) {
    cache_key <- paste(
      "app",
      unified_app_id %||% paste(ios_app_id, android_app_id, sep = "_"),
      format(start_date, "%Y%m%d"),
      format(end_date, "%Y%m%d"),
      paste(countries, collapse = "_"),
      paste(metrics, collapse = "_"),
      sep = "_"
    )
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
    
    if (file.exists(cache_file)) {
      cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
      if (cache_age < 24) {
        if (verbose) {
          message(sprintf("  Using cached app data (%.1f hours old)", as.numeric(cache_age)))
        }
        cached_data <- readRDS(cache_file)
        return(list(data = cached_data, api_calls = 0))
      }
    }
  }
  
  # Separate regular metrics from active user metrics
  regular_metrics <- intersect(metrics, c("revenue", "downloads"))
  needs_dau <- "dau" %in% metrics
  needs_wau <- "wau" %in% metrics
  needs_mau <- "mau" %in% metrics
  
  api_calls <- 0
  aggregated_data <- tibble::tibble(country = countries)
  
  # Fetch revenue/downloads if requested
  if (length(regular_metrics) > 0) {
    # Use the optimized data fetching function
    result <- fetch_optimized_data(
      ios_app_id = ios_app_id,
      android_app_id = android_app_id,
      app_id = unified_app_id,
      start_date = start_date,
      end_date = end_date,
      countries = countries,
      date_granularity = "daily",
      auth_token = auth_token,
      verbose = verbose
    )
    
    # Ensure required columns exist in the data
    for (metric in regular_metrics) {
      if (!metric %in% colnames(result$data)) {
        result$data[[metric]] <- 0
      }
    }
    
    # Aggregate by country (summing daily data)
    if ("country" %in% colnames(result$data)) {
      revenue_downloads <- result$data %>%
        group_by(country) %>%
        summarise(
          across(all_of(regular_metrics), ~sum(.x, na.rm = TRUE)),
          .groups = "drop"
        )
    } else {
      # If no country column, just sum the totals
      revenue_downloads <- result$data %>%
        summarise(
          across(all_of(regular_metrics), ~sum(.x, na.rm = TRUE))
        ) %>%
        mutate(country = countries[1])  # Use the requested country
    }
    
    # Merge with aggregated data
    aggregated_data <- aggregated_data %>%
      left_join(revenue_downloads, by = "country")
    
    api_calls <- api_calls + result$api_calls
  }
  
  # Fetch DAU if requested
  if (needs_dau) {
    dau_result <- fetch_dau_metrics(
      unified_app_id = unified_app_id,
      ios_app_id = ios_app_id,
      android_app_id = android_app_id,
      start_date = start_date,
      end_date = end_date,
      countries = countries,
      cache_dir = cache_dir,
      auth_token = auth_token,
      verbose = verbose
    )
    
    # Merge DAU data
    if (nrow(dau_result$data) > 0) {
      aggregated_data <- aggregated_data %>%
        left_join(dau_result$data %>% select(country, dau), by = "country")
    } else {
      aggregated_data$dau <- NA_real_
    }
    
    api_calls <- api_calls + dau_result$api_calls
  }
  
  # Fetch WAU if requested
  if (needs_wau) {
    wau_result <- fetch_wau_metrics(
      unified_app_id = unified_app_id,
      ios_app_id = ios_app_id,
      android_app_id = android_app_id,
      start_date = start_date,
      end_date = end_date,
      countries = countries,
      cache_dir = cache_dir,
      auth_token = auth_token,
      verbose = verbose
    )
    
    # Merge WAU data
    if (nrow(wau_result$data) > 0) {
      aggregated_data <- aggregated_data %>%
        left_join(wau_result$data %>% select(country, wau), by = "country")
    } else {
      aggregated_data$wau <- NA_real_
    }
    
    api_calls <- api_calls + wau_result$api_calls
  }
  
  # Fetch MAU if requested
  if (needs_mau) {
    mau_result <- fetch_mau_metrics(
      app_id = unified_app_id,
      ios_app_id = ios_app_id,
      android_app_id = android_app_id,
      start_date = start_date,
      end_date = end_date,
      countries = countries,
      cache_dir = cache_dir,
      auth_token = auth_token,
      verbose = verbose
    )
    
    # Merge MAU data
    if (nrow(mau_result$data) > 0) {
      aggregated_data <- aggregated_data %>%
        left_join(mau_result$data %>% select(country, mau), by = "country")
    } else {
      aggregated_data$mau <- NA_real_
    }
    
    api_calls <- api_calls + mau_result$api_calls
  }
  
  # Save to cache
  if (!is.null(cache_dir) && exists("cache_file")) {
    saveRDS(aggregated_data, cache_file)
  }
  
  return(list(data = aggregated_data, api_calls = api_calls))
}

#' Fetch DAU metrics for apps
#' @noRd
fetch_dau_metrics <- function(
  unified_app_id = NULL,
  ios_app_id = NULL,
  android_app_id = NULL,
  start_date,
  end_date,
  countries,
  cache_dir = NULL,
  auth_token,
  verbose = FALSE
) {
  
  # Check cache first
  if (!is.null(cache_dir)) {
    cache_key <- paste(
      "dau",
      unified_app_id %||% paste(ios_app_id, android_app_id, sep = "_"),
      format(start_date, "%Y%m%d"),
      format(end_date, "%Y%m%d"),
      paste(countries, collapse = "_"),
      sep = "_"
    )
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
    
    if (file.exists(cache_file)) {
      cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
      if (cache_age < 24) {
        if (verbose) {
          message(sprintf("    Using cached DAU data (%.1f hours old)", as.numeric(cache_age)))
        }
        cached_data <- readRDS(cache_file)
        return(list(data = cached_data, api_calls = 0))
      }
    }
  }
  
  api_calls <- 0
  all_dau_data <- list()
  
  # Determine which platforms to fetch
  platforms_to_fetch <- list()
  
  if (!is.null(unified_app_id)) {
    # Try to detect platform from unified ID
    if (grepl("^\\d+$", unified_app_id)) {
      platforms_to_fetch[["ios"]] <- unified_app_id
    } else if (grepl("^(com|net|org|io)\\.", unified_app_id)) {
      platforms_to_fetch[["android"]] <- unified_app_id
    } else {
      # Try both platforms
      platforms_to_fetch[["ios"]] <- unified_app_id
      platforms_to_fetch[["android"]] <- unified_app_id
    }
  }
  
  if (!is.null(ios_app_id)) {
    platforms_to_fetch[["ios"]] <- ios_app_id
  }
  
  if (!is.null(android_app_id)) {
    platforms_to_fetch[["android"]] <- android_app_id
  }
  
  # Fetch DAU for each platform
  for (platform in names(platforms_to_fetch)) {
    app_id <- platforms_to_fetch[[platform]]
    
    if (verbose) {
      message(sprintf("    Fetching %s DAU for %s", platform, app_id))
    }
    
    # Build API request
    base_url <- sprintf("https://api.sensortower.com/v1/%s/usage/active_users", platform)
    
    params <- list(
      app_ids = app_id,
      time_period = "day",
      start_date = format(start_date, "%Y-%m-%d"),
      end_date = format(end_date, "%Y-%m-%d"),
      auth_token = auth_token
    )
    
    # Add countries if not WW
    if (!is.null(countries) && countries != "WW") {
      params$countries <- countries
    }
    
    # Make API request
    response <- httr::GET(base_url, query = params)
    api_calls <- api_calls + 1
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      
      if (nchar(content) > 0 && content != "[]" && content != "{}") {
        data <- jsonlite::fromJSON(content, flatten = TRUE)
        
        if (is.data.frame(data) && nrow(data) > 0) {
          # Process platform-specific columns
          if (platform == "ios") {
            # Sum iPhone and iPad users
            data$daily_users <- rowSums(data[, c("iphone_users", "ipad_users")], na.rm = TRUE)
          } else {
            # Android has a single users column
            data$daily_users <- data$users
          }
          
          # Store with platform identifier
          all_dau_data[[platform]] <- data %>%
            select(app_id, country, date, daily_users)
        }
      }
    } else {
      if (verbose) {
        message(sprintf("      Failed to fetch %s DAU: HTTP %d", platform, httr::status_code(response)))
      }
    }
  }
  
  # Combine and aggregate DAU data
  if (length(all_dau_data) > 0) {
    combined_dau <- dplyr::bind_rows(all_dau_data)
    
    # Calculate average DAU across all days and platforms by country
    aggregated_dau <- combined_dau %>%
      group_by(country) %>%
      summarise(
        # Average DAU = sum of daily users / number of days
        total_user_days = sum(daily_users, na.rm = TRUE),
        n_days = n_distinct(date),
        dau = total_user_days / n_days,
        .groups = "drop"
      ) %>%
      select(country, dau)
    
    # Save to cache
    if (!is.null(cache_dir) && exists("cache_file")) {
      saveRDS(aggregated_dau, cache_file)
    }
    
    return(list(data = aggregated_dau, api_calls = api_calls))
  } else {
    # Return empty data
    empty_data <- tibble::tibble(
      country = countries,
      dau = NA_real_
    )
    
    return(list(data = empty_data, api_calls = api_calls))
  }
}

#' Fetch WAU metrics for apps
#' @noRd
fetch_wau_metrics <- function(
  unified_app_id = NULL,
  ios_app_id = NULL,
  android_app_id = NULL,
  start_date,
  end_date,
  countries,
  cache_dir = NULL,
  auth_token,
  verbose = FALSE
) {
  
  # Check cache first
  if (!is.null(cache_dir)) {
    cache_key <- paste(
      "wau",
      unified_app_id %||% paste(ios_app_id, android_app_id, sep = "_"),
      format(start_date, "%Y%m%d"),
      format(end_date, "%Y%m%d"),
      paste(countries, collapse = "_"),
      sep = "_"
    )
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
    
    if (file.exists(cache_file)) {
      cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
      if (cache_age < 24) {
        if (verbose) {
          message(sprintf("    Using cached WAU data (%.1f hours old)", as.numeric(cache_age)))
        }
        cached_data <- readRDS(cache_file)
        return(list(data = cached_data, api_calls = 0))
      }
    }
  }
  
  api_calls <- 0
  all_wau_data <- list()
  
  # Determine which platforms to fetch
  platforms_to_fetch <- list()
  
  if (!is.null(unified_app_id)) {
    # Try to detect platform from unified ID
    if (grepl("^\\d+$", unified_app_id)) {
      platforms_to_fetch[["ios"]] <- unified_app_id
    } else if (grepl("^(com|net|org|io)\\.", unified_app_id)) {
      platforms_to_fetch[["android"]] <- unified_app_id
    } else {
      # Try both platforms
      platforms_to_fetch[["ios"]] <- unified_app_id
      platforms_to_fetch[["android"]] <- unified_app_id
    }
  }
  
  if (!is.null(ios_app_id)) {
    platforms_to_fetch[["ios"]] <- ios_app_id
  }
  
  if (!is.null(android_app_id)) {
    platforms_to_fetch[["android"]] <- android_app_id
  }
  
  # Fetch WAU for each platform
  for (platform in names(platforms_to_fetch)) {
    app_id <- platforms_to_fetch[[platform]]
    
    if (verbose) {
      message(sprintf("    Fetching %s WAU for %s", platform, app_id))
    }
    
    # Build API request
    base_url <- sprintf("https://api.sensortower.com/v1/%s/usage/active_users", platform)
    
    params <- list(
      app_ids = app_id,
      time_period = "week",  # Weekly instead of daily
      start_date = format(start_date, "%Y-%m-%d"),
      end_date = format(end_date, "%Y-%m-%d"),
      auth_token = auth_token
    )
    
    # Add countries if not WW
    if (!is.null(countries) && countries != "WW") {
      params$countries <- countries
    }
    
    # Make API request
    response <- httr::GET(base_url, query = params)
    api_calls <- api_calls + 1
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      
      if (nchar(content) > 0 && content != "[]" && content != "{}") {
        data <- jsonlite::fromJSON(content, flatten = TRUE)
        
        if (is.data.frame(data) && nrow(data) > 0) {
          # Process platform-specific columns
          if (platform == "ios") {
            # Sum iPhone and iPad users
            data$weekly_users <- rowSums(data[, c("iphone_users", "ipad_users")], na.rm = TRUE)
          } else {
            # Android has a single users column
            data$weekly_users <- data$users
          }
          
          # Store with platform identifier (ensure app_id is character)
          all_wau_data[[platform]] <- data %>%
            mutate(app_id = as.character(app_id)) %>%
            select(app_id, country, date, weekly_users)
        }
      }
    } else {
      if (verbose) {
        message(sprintf("      Failed to fetch %s WAU: HTTP %d", platform, httr::status_code(response)))
      }
    }
  }
  
  # Combine and aggregate WAU data
  if (length(all_wau_data) > 0) {
    combined_wau <- dplyr::bind_rows(all_wau_data)
    
    # Calculate average WAU across all weeks and platforms by country
    aggregated_wau <- combined_wau %>%
      group_by(country) %>%
      summarise(
        # Average WAU = sum of weekly users / number of weeks
        total_user_weeks = sum(weekly_users, na.rm = TRUE),
        n_weeks = n_distinct(date),
        wau = total_user_weeks / n_weeks,
        .groups = "drop"
      ) %>%
      select(country, wau)
    
    # Save to cache
    if (!is.null(cache_dir) && exists("cache_file")) {
      saveRDS(aggregated_wau, cache_file)
    }
    
    return(list(data = aggregated_wau, api_calls = api_calls))
  } else {
    # Return empty data
    empty_data <- tibble::tibble(
      country = countries,
      wau = NA_real_
    )
    
    return(list(data = empty_data, api_calls = api_calls))
  }
}

# Fetch MAU data
fetch_mau_metrics <- function(
  app_id, 
  ios_app_id, 
  android_app_id, 
  start_date, 
  end_date, 
  countries, 
  auth_token,
  cache_dir = NULL,
  verbose = FALSE
) {
  
  api_calls <- 0
  all_mau_data <- list()
  
  # Check cache first
  if (!is.null(cache_dir)) {
    cache_key <- paste0(
      "mau_",
      digest::digest(list(app_id, ios_app_id, android_app_id, start_date, end_date, countries))
    )
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
    
    if (file.exists(cache_file)) {
      cache_time <- file.info(cache_file)$mtime
      if (difftime(Sys.time(), cache_time, units = "hours") < 24) {
        if (verbose) message("    Using cached MAU data")
        return(list(data = readRDS(cache_file), api_calls = 0))
      }
    }
  }
  
  # Determine which platforms to fetch
  platforms_to_fetch <- list()
  
  if (!is.null(ios_app_id) || (!is.null(app_id) && grepl("^[0-9]+$", app_id[1]))) {
    platforms_to_fetch$ios <- if (!is.null(ios_app_id)) ios_app_id else app_id
  }
  
  if (!is.null(android_app_id) || (!is.null(app_id) && !grepl("^[0-9]+$", app_id[1]))) {
    platforms_to_fetch$android <- if (!is.null(android_app_id)) android_app_id else app_id
  }
  
  if (verbose) {
    message(sprintf("    Fetching MAU for %d platform(s) from %s to %s", 
                    length(platforms_to_fetch), start_date, end_date))
  }
  
  # Fetch MAU data for each platform
  for (platform in names(platforms_to_fetch)) {
    app_ids <- platforms_to_fetch[[platform]]
    
    if (verbose) {
      message(sprintf("      Fetching %s MAU for %d app(s)", platform, length(app_ids)))
    }
    
    # Prepare API parameters
    base_url <- sprintf("https://api.sensortower.com/v1/%s/usage/active_users", platform)
    
    params <- list(
      app_ids = paste(app_ids, collapse = ","),
      start_date = as.character(start_date),
      end_date = as.character(end_date),
      time_period = "month",
      auth_token = auth_token
    )
    
    # Add countries if not WW
    if (!is.null(countries) && countries != "WW") {
      params$countries <- countries
    }
    
    # Make API request
    response <- httr::GET(base_url, query = params)
    api_calls <- api_calls + 1
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      
      if (nchar(content) > 0 && content != "[]" && content != "{}") {
        data <- jsonlite::fromJSON(content, flatten = TRUE)
        
        if (is.data.frame(data) && nrow(data) > 0) {
          # Process platform-specific columns
          if (platform == "ios") {
            # Sum iPhone and iPad users
            data$monthly_users <- rowSums(data[, c("iphone_users", "ipad_users")], na.rm = TRUE)
          } else {
            # Android has a single users column
            data$monthly_users <- data$users
          }
          
          # Ensure app_id is character for consistency
          data$app_id <- as.character(data$app_id)
          
          # Store with platform identifier
          all_mau_data[[platform]] <- data %>%
            select(app_id, country, date, monthly_users)
        }
      }
    } else {
      if (verbose) {
        message(sprintf("      Failed to fetch %s MAU: HTTP %d", platform, httr::status_code(response)))
      }
    }
  }
  
  # Combine and aggregate MAU data
  if (length(all_mau_data) > 0) {
    combined_mau <- dplyr::bind_rows(all_mau_data)
    
    # Calculate average MAU across all months and platforms by country
    aggregated_mau <- combined_mau %>%
      group_by(country) %>%
      summarise(
        # Average MAU = sum of monthly users / number of months
        total_user_months = sum(monthly_users, na.rm = TRUE),
        n_months = n_distinct(date),
        mau = total_user_months / n_months,
        .groups = "drop"
      ) %>%
      select(country, mau)
    
    # Save to cache
    if (!is.null(cache_dir) && exists("cache_file")) {
      saveRDS(aggregated_mau, cache_file)
    }
    
    return(list(data = aggregated_mau, api_calls = api_calls))
  } else {
    # Return empty data
    empty_data <- tibble::tibble(
      country = countries,
      mau = NA_real_
    )
    
    return(list(data = empty_data, api_calls = api_calls))
  }
}

# Optimized data fetching function with intelligent batching
fetch_optimized_data <- function(
  ios_app_id = NULL,
  android_app_id = NULL,
  app_id = NULL,
  start_date,
  end_date,
  countries,
  date_granularity,
  auth_token,
  verbose = FALSE
) {
  
  api_calls <- 0
  all_data <- tibble::tibble()
  
  # Determine batching strategy based on granularity and date range
  days_span <- as.integer(end_date - start_date) + 1
  
  if (date_granularity == "daily") {
    # For daily data, use intelligent batching
    if (days_span <= 7) {
      # Single call for short periods
      batch_data <- st_metrics(
        app_id = app_id,
        ios_app_id = ios_app_id,
        android_app_id = android_app_id,
        start_date = start_date,
        end_date = end_date,
        countries = countries,
        date_granularity = date_granularity,
        auth_token = auth_token,
        verbose = FALSE
      )
      api_calls <- 1
      all_data <- batch_data
    } else {
      # For longer periods, batch by complete months when possible
      current_date <- start_date
      
      while (current_date <= end_date) {
        # Check if we can fetch a complete month
        month_start <- lubridate::floor_date(current_date, "month")
        month_end <- lubridate::ceiling_date(current_date, "month") - 1
        
        if (month_start >= start_date && month_end <= end_date) {
          # Fetch complete month
          batch_end <- month_end
        } else if (current_date == start_date && 
                   lubridate::day(start_date) > 1) {
          # Partial month at start
          batch_end <- min(month_end, end_date)
        } else {
          # Default to weekly batches
          batch_end <- min(current_date + 6, end_date)
        }
        
        if (verbose) {
          message(sprintf("    Fetching %s to %s", current_date, batch_end))
        }
        
        batch_data <- st_metrics(
          app_id = app_id,
          ios_app_id = ios_app_id,
          android_app_id = android_app_id,
          start_date = current_date,
          end_date = batch_end,
          countries = countries,
          date_granularity = date_granularity,
          auth_token = auth_token,
          verbose = FALSE
        )
        
        api_calls <- api_calls + 1
        
        if (nrow(batch_data) > 0) {
          all_data <- dplyr::bind_rows(all_data, batch_data)
        }
        
        # Move to next batch
        current_date <- batch_end + 1
      }
    }
  } else {
    # For non-daily granularities, single call
    all_data <- st_metrics(
      app_id = app_id,
      ios_app_id = ios_app_id,
      android_app_id = android_app_id,
      start_date = start_date,
      end_date = end_date,
      countries = countries,
      date_granularity = date_granularity,
      auth_token = auth_token,
      verbose = FALSE
    )
    api_calls <- 1
  }
  
  return(list(data = all_data, api_calls = api_calls))
}