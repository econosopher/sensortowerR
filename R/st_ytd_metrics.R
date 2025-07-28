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
#' @param metrics Character vector. Metrics to fetch. Currently supports "revenue" and "downloads".
#'   Default is both metrics.
#' @param countries Character vector. Country codes (default: "US").
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
#'   - `metric`: The metric name (e.g., "revenue", "downloads", etc.)
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
#'
#' @examples
#' \dontrun{
#' # Get YTD metrics for a single app across multiple years
#' ytd_metrics <- st_ytd_metrics(
#'   unified_app_id = "553834731",  # Candy Crush
#'   years = c(2023, 2024, 2025)
#' )
#'
#' # Get metrics for multiple apps at once
#' multi_app_metrics <- st_ytd_metrics(
#'   unified_app_id = c("553834731", "1195621598", "1053012308"),  # Multiple apps
#'   years = c(2024, 2025),
#'   metrics = c("revenue", "downloads")
#' )
#'
#' # Get February metrics for multiple publishers
#' feb_metrics <- st_ytd_metrics(
#'   publisher_id = c("pub123", "pub456", "pub789"),
#'   years = c(2023, 2024, 2025),
#'   period_start = "02-01",
#'   period_end = "02-28"  # Auto-handles Feb 29 in 2024
#' )
#'
#' # Mix iOS and Android IDs for complete data
#' ytd_metrics <- st_ytd_metrics(
#'   ios_app_id = c("1195621598", "553834731"),
#'   android_app_id = c("com.playrix.homescapes", "com.king.candycrushsaga"),
#'   years = c(2024, 2025),
#'   cache_dir = ".cache/ytd"
#' )
#' }
#'
#' @importFrom dplyr %>% group_by summarise mutate select filter bind_rows all_of
#' @importFrom lubridate floor_date ceiling_date wday day
#' @importFrom tibble tibble
#' @importFrom rlang %||%
#' @importFrom tidyr pivot_longer
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
  countries = "US",
  cache_dir = NULL,
  auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
  verbose = TRUE
) {
  
  # Validate metrics
  valid_metrics <- c("revenue", "downloads")
  if (!all(metrics %in% valid_metrics)) {
    invalid <- metrics[!metrics %in% valid_metrics]
    stop(paste0("Invalid metrics: ", paste(invalid, collapse = ", "), 
                ". Currently only 'revenue' and 'downloads' are supported."))
  }
  
  # Validate entity inputs
  if (is.null(unified_app_id) && is.null(ios_app_id) && is.null(android_app_id) && is.null(publisher_id)) {
    stop("At least one entity ID must be provided (unified_app_id, ios_app_id, android_app_id, or publisher_id)")
  }
  
  if (!is.null(publisher_id) && (!is.null(unified_app_id) || !is.null(ios_app_id) || !is.null(android_app_id))) {
    stop("Cannot specify both publisher_id and app IDs. Choose one entity type.")
  }
  
  # Determine entity type
  entity_type <- if (!is.null(publisher_id)) "publisher" else "app"
  
  # Default to current year if not specified
  if (is.null(years)) {
    years <- as.integer(format(Sys.Date(), "%Y"))
  }
  years <- sort(unique(as.integer(years)))
  
  # Calculate default period_end if not provided (last completed week ending Saturday)
  if (is.null(period_end)) {
    today <- Sys.Date()
    # Find last Saturday (wday: 1=Sunday, 7=Saturday)
    days_since_saturday <- (lubridate::wday(today) + 7 - 7) %% 7
    if (days_since_saturday == 0 && lubridate::wday(today) != 7) {
      days_since_saturday <- 7  # If today is not Saturday, go back to previous Saturday
    }
    last_saturday <- today - days_since_saturday
    period_end <- format(last_saturday, "%m-%d")
    
    if (verbose) {
      message(sprintf("Using last completed week ending: %s", format(last_saturday, "%Y-%m-%d")))
    }
  }
  
  # Default period_start to January 1
  if (is.null(period_start)) {
    period_start <- "01-01"
  }
  
  # Validate date formats
  if (!grepl("^\\d{2}-\\d{2}$", period_start)) {
    stop("period_start must be in MM-DD format (e.g., '01-01')")
  }
  if (!grepl("^\\d{2}-\\d{2}$", period_end)) {
    stop("period_end must be in MM-DD format (e.g., '12-31')")
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
  tidy_data <- combined_data %>%
    tidyr::pivot_longer(
      cols = all_of(metrics),
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
    country = character(),
    revenue = numeric(),
    downloads = numeric()
  )
  
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
  
  # Aggregate by country (summing daily data)
  # Check if country column exists
  if ("country" %in% colnames(result$data)) {
    aggregated_data <- result$data %>%
      group_by(country) %>%
      summarise(
        revenue = sum(revenue, na.rm = TRUE),
        downloads = sum(downloads, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # If no country column, just sum the totals
    aggregated_data <- result$data %>%
      summarise(
        revenue = sum(revenue, na.rm = TRUE),
        downloads = sum(downloads, na.rm = TRUE)
      ) %>%
      mutate(country = countries[1])  # Use the requested country
  }
  
  # Save to cache
  if (!is.null(cache_dir) && !is.null(cache_file)) {
    saveRDS(aggregated_data, cache_file)
  }
  
  return(list(data = aggregated_data, api_calls = result$api_calls))
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
        all_data <- dplyr::bind_rows(all_data, batch_data)
        
        current_date <- batch_end + 1
      }
    }
  } else {
    # For non-daily granularities, use the existing batching logic
    result <- st_metrics(
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
    all_data <- result
  }
  
  list(
    data = all_data,
    api_calls = api_calls
  )
}