#' Fetch Sensor Tower Metrics for Apps
#'
#' Retrieves metrics for apps. First attempts to use the unified endpoint,
#' but if it returns empty (which happens for all granularities currently),
#' automatically falls back to platform-specific endpoints and combines the results.
#'
#' @param app_id Character string. Can be a unified app ID, iOS app ID, or Android package name.
#' @param ios_app_id Character string. iOS app ID (optional, improves data completeness).
#' @param android_app_id Character string. Android package name (optional, improves data completeness).
#' @param unified_app_id Deprecated. Use `app_id` instead.
#' @param start_date Date object or character string (YYYY-MM-DD). Start date.
#' @param end_date Date object or character string (YYYY-MM-DD). End date.
#' @param countries Character vector. Country codes (default "US").
#' @param date_granularity Character. One of "daily", "weekly", "monthly", "quarterly".
#' @param auto_platform_fetch Logical. When TRUE (default), automatically falls back
#'   to platform-specific endpoints if unified endpoint returns no data.
#' @param combine_platforms Logical. When TRUE (default), combines iOS and Android data
#'   into unified totals. The platform column is excluded from results when TRUE.
#' @param auth_token Character string. Sensor Tower API token.
#' @param verbose Logical. Print progress messages.
#'
#' @return A tibble with columns: date, country, revenue, downloads, platform (if not combined)
#'
#' @details
#' This function intelligently handles Sensor Tower's API limitations:
#' 
#' - First attempts the unified sales_report_estimates endpoint
#' - If unified returns empty (current behavior for all granularities), falls back to platform-specific endpoints
#' - Automatically detects app ID format and fetches appropriate data
#' - Combines iOS and Android data for true unified view when using fallback
#' - Set auto_platform_fetch = FALSE to disable automatic fallback
#'
#' @examples
#' \dontrun{
#' # Simple usage - function figures out the platform
#' metrics <- st_metrics(
#'   app_id = "1195621598",  # Homescapes iOS
#'   start_date = Sys.Date() - 30,
#'   end_date = Sys.Date() - 1
#' )
#'
#' # Best practice - provide both platform IDs
#' metrics <- st_metrics(
#'   app_id = "1195621598",
#'   ios_app_id = "1195621598",
#'   android_app_id = "com.playrix.homescapes",
#'   start_date = Sys.Date() - 30,
#'   end_date = Sys.Date() - 1
#' )
#'
#' # Keep platforms separate
#' metrics <- st_metrics(
#'   app_id = "com.king.candycrushsaga",
#'   combine_platforms = FALSE
#' )
#'
#' # Force unified endpoint (will fail for daily)
#' metrics <- st_metrics(
#'   app_id = "1195621598",
#'   date_granularity = "monthly",
#'   auto_platform_fetch = FALSE
#' )
#' }
#'
#' @importFrom lubridate floor_date
#' @importFrom dplyr %>% mutate select bind_rows group_by summarise coalesce
#' @importFrom tibble tibble
#' @importFrom rlang %||% .data
#' @export
st_metrics <- function(
  app_id = NULL,
  ios_app_id = NULL,
  android_app_id = NULL,
  unified_app_id = NULL,  # Deprecated parameter
  start_date = NULL,
  end_date = NULL,
  countries = "US",
  date_granularity = "daily",
  auto_platform_fetch = TRUE,
  combine_platforms = TRUE,
  auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
  verbose = TRUE
) {
  
  # Handle deprecated parameter
  if (!is.null(unified_app_id) && is.null(app_id)) {
    warning("Parameter 'unified_app_id' is deprecated. Use 'app_id' instead.")
    app_id <- unified_app_id
  }
  
  # Validate inputs
  if (is.null(app_id) && is.null(ios_app_id) && is.null(android_app_id)) {
    stop("At least one app ID must be provided (app_id, ios_app_id, or android_app_id)")
  }
  
  # If only app_id provided, use it as the primary ID
  if (is.null(ios_app_id) && is.null(android_app_id)) {
    if (!is.null(app_id)) {
      # Detect platform from app_id format
      if (grepl("^\\d+$", app_id)) {
        ios_app_id <- app_id
        if (verbose) message("Detected iOS app ID format")
      } else if (grepl("^(com|net|org|io)\\.", app_id)) {
        android_app_id <- app_id
        if (verbose) message("Detected Android package name format")
      } else {
        warning("Could not determine platform from app_id format. Results may be incomplete.")
      }
    }
  }
  
  # Handle dates
  if (is.null(start_date)) {
    start_date <- lubridate::floor_date(Sys.Date(), "month")
  }
  if (is.null(end_date)) {
    end_date <- Sys.Date()
  }
  
  # Convert dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if (start_date > end_date) {
    stop("'start_date' must be earlier than or equal to 'end_date'")
  }
  
  # Check authentication
  if (is.null(auth_token) || auth_token == "") {
    stop("Authentication token required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  # First try unified endpoint
  if (verbose) {
    message("Attempting unified endpoint...")
  }
  
  # Try unified endpoint first
  unified_result <- tryCatch({
    fetch_unified_metrics(
      app_id = app_id %||% ios_app_id %||% android_app_id,
      start_date = start_date,
      end_date = end_date,
      countries = countries,
      date_granularity = date_granularity,
      auth_token = auth_token
    )
  }, error = function(e) {
    if (verbose) {
      message(sprintf("Unified endpoint error: %s", e$message))
    }
    NULL
  })
  
  # Check if unified returned data
  if (!is.null(unified_result) && nrow(unified_result) > 0) {
    if (verbose) {
      message(sprintf("Unified endpoint successful: %d rows returned", nrow(unified_result)))
    }
    return(unified_result)
  }
  
  # If unified failed or returned empty, use platform-specific
  if (auto_platform_fetch) {
    if (verbose) {
      message("Unified endpoint returned no data. Switching to platform-specific endpoints...")
    }
    return(fetch_platform_specific_data(
      ios_app_id = ios_app_id,
      android_app_id = android_app_id,
      start_date = start_date,
      end_date = end_date,
      countries = countries,
      date_granularity = date_granularity,
      combine_platforms = combine_platforms,
      auth_token = auth_token,
      verbose = verbose
    ))
  } else {
    # If auto_platform_fetch is disabled and unified failed
    if (verbose) {
      warning("Unified endpoint returned no data. Set auto_platform_fetch = TRUE to use platform-specific endpoints as fallback.")
    }
    
    # Return empty result
    return(tibble::tibble(
      date = as.Date(character()),
      country = character(),
      revenue = numeric(),
      downloads = numeric()
    ))
  }
}

# Helper function for platform-specific fetching
fetch_platform_specific_data <- function(
  ios_app_id = NULL,
  android_app_id = NULL,
  start_date,
  end_date,
  countries,
  date_granularity,
  combine_platforms,
  auth_token,
  verbose
) {
  
  all_data <- tibble::tibble()
  
  # Fetch iOS data
  if (!is.null(ios_app_id) && ios_app_id != "") {
    if (verbose) message(sprintf("Fetching iOS data for %s...", ios_app_id))
    
    ios_data <- tryCatch({
      st_sales_report(
        app_ids = ios_app_id,
        os = "ios",
        countries = countries,
        start_date = start_date,
        end_date = end_date,
        date_granularity = date_granularity,
        auth_token = auth_token,
        auto_segment = TRUE,
        verbose = FALSE
      )
    }, error = function(e) {
      warning(paste("Failed to fetch iOS data:", e$message))
      return(NULL)
    })
    
    if (!is.null(ios_data) && nrow(ios_data) > 0) {
      # Standardize columns
      ios_clean <- ios_data %>%
        dplyr::mutate(
          platform = "iOS",
          revenue = dplyr::coalesce(
            total_revenue,
            iphone_revenue + ipad_revenue,
            0
          ),
          downloads = dplyr::coalesce(
            total_downloads,
            iphone_downloads + ipad_downloads,
            0
          )
        ) %>%
        dplyr::select(date, country, revenue, downloads, platform)
      
      all_data <- dplyr::bind_rows(all_data, ios_clean)
      if (verbose) message(sprintf("  Retrieved %d iOS records", nrow(ios_clean)))
    }
  }
  
  # Fetch Android data
  if (!is.null(android_app_id) && android_app_id != "") {
    if (verbose) message(sprintf("Fetching Android data for %s...", android_app_id))
    
    android_data <- tryCatch({
      st_sales_report(
        app_ids = android_app_id,
        os = "android",
        countries = countries,
        start_date = start_date,
        end_date = end_date,
        date_granularity = date_granularity,
        auth_token = auth_token,
        auto_segment = TRUE,
        verbose = FALSE
      )
    }, error = function(e) {
      warning(paste("Failed to fetch Android data:", e$message))
      return(NULL)
    })
    
    if (!is.null(android_data) && nrow(android_data) > 0) {
      # Standardize columns - Android already has revenue/downloads columns
      android_clean <- android_data %>%
        dplyr::mutate(
          platform = "Android",
          country = c  # Android returns 'c' instead of 'country'
        ) %>%
        dplyr::select(date, country, revenue, downloads, platform)
      
      all_data <- dplyr::bind_rows(all_data, android_clean)
      if (verbose) message(sprintf("  Retrieved %d Android records", nrow(android_clean)))
    }
  }
  
  # Combine platforms if requested (default behavior for unified view)
  if (combine_platforms && nrow(all_data) > 0) {
    all_data <- all_data %>%
      dplyr::group_by(date, country) %>%
      dplyr::summarise(
        revenue = sum(revenue, na.rm = TRUE),
        downloads = sum(downloads, na.rm = TRUE),
        .groups = "drop"
      )
    # Note: platform column is intentionally excluded for unified view
  }
  
  if (nrow(all_data) == 0) {
    warning("No data retrieved. Check app IDs and date range.")
  }
  
  return(all_data)
}

# Helper function for unified endpoint
fetch_unified_metrics <- function(app_id, start_date, end_date, countries, date_granularity, auth_token) {
  
  # Build the API request
  base_url <- "https://api.sensortower.com/v1/unified/sales_report_estimates"
  
  params <- list(
    app_ids = app_id,
    countries = paste(countries, collapse = ","),
    date_granularity = date_granularity,
    start_date = format(start_date, "%Y-%m-%d"),
    end_date = format(end_date, "%Y-%m-%d"),
    auth_token = auth_token
  )
  
  # Make the API request
  response <- httr::GET(base_url, query = params)
  
  # Check status
  if (httr::status_code(response) != 200) {
    stop(sprintf("API error: HTTP %d", httr::status_code(response)))
  }
  
  # Parse response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  
  if (nchar(content) == 0 || content == "[]" || content == "{}") {
    # Empty response
    return(tibble::tibble(
      date = as.Date(character()),
      country = character(),
      revenue = numeric(),
      downloads = numeric()
    ))
  }
  
  # Parse JSON
  result <- jsonlite::fromJSON(content, flatten = TRUE)
  
  # Convert to tibble and standardize columns
  if (is.data.frame(result) && nrow(result) > 0) {
    result_tbl <- tibble::as_tibble(result)
    
    # Standardize column names
    if ("c" %in% names(result_tbl)) {
      result_tbl$country <- result_tbl$c
    }
    
    # Ensure required columns exist
    required_cols <- c("date", "country", "revenue", "downloads")
    missing_cols <- setdiff(required_cols, names(result_tbl))
    
    if (length(missing_cols) > 0) {
      # Try to find revenue/download columns with different names
      if ("total_revenue" %in% names(result_tbl)) {
        result_tbl$revenue <- result_tbl$total_revenue
      }
      if ("total_downloads" %in% names(result_tbl)) {
        result_tbl$downloads <- result_tbl$total_downloads
      }
    }
    
    # Select only required columns
    if (all(c("date", "country", "revenue", "downloads") %in% names(result_tbl))) {
      return(dplyr::select(result_tbl, date, country, revenue, downloads))
    }
  }
  
  # If we got here, something went wrong
  return(tibble::tibble(
    date = as.Date(character()),
    country = character(),
    revenue = numeric(),
    downloads = numeric()
  ))
}