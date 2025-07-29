#' Fetch Sensor Tower Metrics for Apps
#'
#' Retrieves metrics for apps. The OS parameter controls which platform's data is returned,
#' regardless of which app IDs are provided. The function will automatically look up
#' the appropriate IDs if needed.
#'
#' @param os Character. Required. Operating system: "ios", "android", or "unified".
#'   This determines which platform's data is returned.
#' @param app_id Character string. Can be a unified app ID, iOS app ID, or Android package name.
#' @param ios_app_id Character string. iOS app ID (optional).
#' @param android_app_id Character string. Android package name (optional).
#' @param unified_app_id Character string. Sensor Tower unified ID (24-char hex).
#' @param start_date Date object or character string (YYYY-MM-DD). Start date.
#' @param end_date Date object or character string (YYYY-MM-DD). End date.
#' @param countries Character vector. Country codes (e.g., "US", "GB", "JP", or "WW" for worldwide). Required.
#' @param date_granularity Character. One of "daily", "weekly", "monthly", "quarterly". Required.
#' @param auth_token Character string. Sensor Tower API token.
#' @param verbose Logical. Print progress messages.
#'
#' @return A tibble with columns: app_id, app_id_type, date, country, revenue, downloads
#'
#' @details
#' The OS parameter controls what data is returned:
#' 
#' - os = "ios": Returns iOS data only
#' - os = "android": Returns Android data only
#' - os = "unified": Returns combined iOS + Android data
#' 
#' The function will automatically look up the appropriate IDs based on the OS parameter.
#' For example, if you provide a unified_app_id but set os = "ios", it will look up
#' the iOS app ID and return iOS-only data.
#'
#' @examples
#' \dontrun{
#' # Get iOS data only
#' ios_metrics <- st_metrics(
#'   os = "ios",
#'   ios_app_id = "1195621598",  # Homescapes iOS
#'   countries = "US",
#'   date_granularity = "daily",
#'   start_date = Sys.Date() - 30,
#'   end_date = Sys.Date() - 1
#' )
#'
#' # Get unified data from a unified ID
#' unified_metrics <- st_metrics(
#'   os = "unified",
#'   unified_app_id = "5ba4585f539ce75b97db6bcb",
#'   countries = "US",
#'   date_granularity = "daily"
#' )
#'
#' # Get iOS data from Android ID (automatic lookup)
#' ios_from_android <- st_metrics(
#'   os = "ios",
#'   android_app_id = "com.king.candycrushsaga",
#'   countries = "WW",
#'   date_granularity = "monthly"
#' )
#'
#' # Get unified data from platform IDs
#' unified_from_platforms <- st_metrics(
#'   os = "unified",
#'   ios_app_id = "1195621598",
#'   android_app_id = "com.playrix.homescapes",
#'   countries = "US",
#'   date_granularity = "daily"
#' )
#' }
#'
#' @importFrom lubridate floor_date
#' @importFrom dplyr %>% mutate select bind_rows group_by summarise coalesce
#' @importFrom tibble tibble
#' @importFrom rlang %||% .data
#' @export
st_metrics <- function(
  os,
  app_id = NULL,
  ios_app_id = NULL,
  android_app_id = NULL,
  unified_app_id = NULL,
  start_date = NULL,
  end_date = NULL,
  countries,
  date_granularity,
  auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
  verbose = TRUE
) {
  
  # Validate OS parameter
  if (missing(os) || is.null(os) || !os %in% c("ios", "android", "unified")) {
    stop("'os' parameter is required and must be one of: 'ios', 'android', or 'unified'")
  }
  
  # Validate required parameters
  if (missing(countries) || is.null(countries) || length(countries) == 0) {
    stop("'countries' parameter is required. Specify country codes (e.g., 'US', 'GB', 'JP', or 'WW' for worldwide).")
  }
  
  if (missing(date_granularity) || is.null(date_granularity)) {
    stop("'date_granularity' parameter is required. Specify one of: 'daily', 'weekly', 'monthly', 'quarterly'.")
  }
  
  # Validate date_granularity value
  valid_granularities <- c("daily", "weekly", "monthly", "quarterly")
  if (!date_granularity %in% valid_granularities) {
    stop(paste0("Invalid date_granularity: '", date_granularity, "'. Must be one of: ", paste(valid_granularities, collapse = ", ")))
  }
  
  # Handle app_id parameter - try to determine what type it is
  if (!is.null(app_id)) {
    if (grepl("^\\d+$", app_id) && is.null(ios_app_id)) {
      ios_app_id <- app_id
      if (verbose) message("Detected iOS app ID format in app_id parameter")
    } else if (grepl("^(com|net|org|io)\\.", app_id) && is.null(android_app_id)) {
      android_app_id <- app_id
      if (verbose) message("Detected Android package name format in app_id parameter")
    } else if (grepl("^[a-f0-9]{24}$", app_id) && is.null(unified_app_id)) {
      unified_app_id <- app_id
      if (verbose) message("Detected unified app ID format in app_id parameter")
    }
  }
  
  # Resolve IDs based on OS parameter
  id_resolution <- resolve_ids_for_os(
    unified_app_id = unified_app_id,
    ios_app_id = ios_app_id,
    android_app_id = android_app_id,
    os = os,
    auth_token = auth_token,
    verbose = verbose
  )
  
  resolved_ids <- id_resolution$resolved_ids
  app_id_type <- id_resolution$app_id_type
  
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
  
  # Now fetch data based on the resolved IDs and OS
  result <- NULL
  
  if (os == "unified") {
    # For unified data, we need both iOS and Android IDs
    # Users must provide either:
    # 1. Both iOS and Android IDs directly, OR
    # 2. A unified ID that we can resolve to both platforms
    
    final_ios_id <- ios_app_id
    final_android_id <- android_app_id
    
    # If we have a unified ID, look up the platform IDs
    if (!is.null(unified_app_id)) {
      if (verbose) message("Looking up platform IDs from unified ID...")
      
      lookup_result <- tryCatch({
        st_app_lookup(unified_app_id, auth_token = auth_token, verbose = FALSE)
      }, error = function(e) {
        stop(paste("Failed to lookup app details for unified ID", unified_app_id, ":", e$message))
      })
      
      if (!is.null(lookup_result)) {
        # Override any provided platform IDs with the lookup results
        final_ios_id <- lookup_result$ios_app_id
        final_android_id <- lookup_result$android_app_id
        
        if (verbose) {
          message("  App name: ", lookup_result$app_name)
          message("  iOS ID: ", final_ios_id %||% "not found")
          message("  Android ID: ", final_android_id %||% "not found")
        }
      }
    }
    
    # Validate we have both platform IDs
    if (is.null(final_ios_id) || is.null(final_android_id)) {
      missing <- c()
      if (is.null(final_ios_id)) missing <- c(missing, "iOS")
      if (is.null(final_android_id)) missing <- c(missing, "Android")
      
      stop(sprintf(
        "Cannot fetch unified data: %s app ID(s) missing. For unified data, you must provide either:\n  1. Both ios_app_id and android_app_id, OR\n  2. A unified_app_id that resolves to both platforms\n\nThis app may not be available on all platforms.",
        paste(missing, collapse = " and ")
      ))
    }
    
    # Fetch and combine data from both platforms
    if (verbose) message("Fetching unified data by combining iOS and Android...")
    
    result <- fetch_and_combine_platforms(
      ios_app_id = final_ios_id,
      android_app_id = final_android_id,
      start_date = start_date,
      end_date = end_date,
      countries = countries,
      date_granularity = date_granularity,
      auth_token = auth_token,
      verbose = verbose
    )
  } else if (os == "ios") {
    # Fetch iOS data only
    if (!is.null(resolved_ids$ios_app_id)) {
      if (verbose) message("Fetching iOS data for: ", resolved_ids$ios_app_id)
      
      result <- tryCatch({
        st_sales_report(
          app_ids = resolved_ids$ios_app_id,
          os = "ios",
          countries = countries,
          start_date = start_date,
          end_date = end_date,
          date_granularity = date_granularity,
          auth_token = auth_token
        )
      }, error = function(e) {
        if (verbose) message("iOS fetch error: ", e$message)
        NULL
      })
    }
  } else if (os == "android") {
    # Fetch Android data only
    if (!is.null(resolved_ids$android_app_id)) {
      if (verbose) message("Fetching Android data for: ", resolved_ids$android_app_id)
      
      result <- tryCatch({
        st_sales_report(
          app_ids = resolved_ids$android_app_id,
          os = "android",
          countries = countries,
          start_date = start_date,
          end_date = end_date,
          date_granularity = date_granularity,
          auth_token = auth_token
        )
      }, error = function(e) {
        if (verbose) message("Android fetch error: ", e$message)
        NULL
      })
    }
  }
  
  # Handle empty results
  if (is.null(result) || nrow(result) == 0) {
    if (verbose) message("No data returned")
    result <- tibble::tibble(
      date = as.Date(character()),
      country = character(),
      revenue = numeric(),
      downloads = numeric()
    )
  } else {
    # Standardize iOS data if needed
    if (os == "ios" && "total_revenue" %in% names(result)) {
      result <- result %>%
        dplyr::mutate(
          revenue = total_revenue,
          downloads = if ("total_downloads" %in% names(.)) total_downloads else 0
        ) %>%
        dplyr::select(date, country, revenue, downloads)
    } else if (os == "android" && "c" %in% names(result)) {
      # Fix Android country column
      result <- result %>%
        dplyr::mutate(country = c) %>%
        dplyr::select(date, country, revenue, downloads)
    }
  }
  
  # Add app_id and app_id_type metadata
  if (!is.null(resolved_ids)) {
    app_id_value <- resolved_ids[[1]]  # Get the first (and only) ID
    result <- add_app_id_metadata(result, app_id_value, app_id_type)
  }
  
  return(result)
}

# Helper function to fetch and combine platform data
fetch_and_combine_platforms <- function(
  ios_app_id = NULL,
  android_app_id = NULL,
  start_date,
  end_date,
  countries,
  date_granularity,
  auth_token,
  verbose
) {
  all_data <- tibble::tibble()
  
  # Track what we requested vs what we got
  ios_requested <- !is.null(ios_app_id)
  android_requested <- !is.null(android_app_id)
  ios_has_data <- FALSE
  android_has_data <- FALSE
  
  # Fetch iOS data
  if (ios_requested) {
    ios_result <- tryCatch({
      st_sales_report(
        app_ids = ios_app_id,
        os = "ios",
        countries = countries,
        start_date = start_date,
        end_date = end_date,
        date_granularity = date_granularity,
        auth_token = auth_token
      )
    }, error = function(e) NULL)
    
    if (!is.null(ios_result) && nrow(ios_result) > 0) {
      ios_has_data <- TRUE
      # Ensure app_id is character to prevent type conflicts
      if ("app_id" %in% names(ios_result)) {
        ios_result$app_id <- as.character(ios_result$app_id)
      }
      all_data <- bind_rows(all_data, ios_result)
    }
  }
  
  # Fetch Android data
  if (android_requested) {
    android_result <- tryCatch({
      st_sales_report(
        app_ids = android_app_id,
        os = "android",
        countries = countries,
        start_date = start_date,
        end_date = end_date,
        date_granularity = date_granularity,
        auth_token = auth_token
      )
    }, error = function(e) NULL)
    
    if (!is.null(android_result) && nrow(android_result) > 0) {
      android_has_data <- TRUE
      # Ensure app_id is character to prevent type conflicts
      if ("app_id" %in% names(android_result)) {
        android_result$app_id <- as.character(android_result$app_id)
      }
      all_data <- bind_rows(all_data, android_result)
    }
  }
  
  # For unified requests, both platforms must have data
  if (ios_requested && android_requested) {
    if (!ios_has_data && !android_has_data) {
      stop("No data available for either iOS or Android platform")
    } else if (!ios_has_data) {
      stop("Unified data requested but iOS data is not available")
    } else if (!android_has_data) {
      stop("Unified data requested but Android data is not available")
    }
  }
  
  # Combine by date and country
  if (nrow(all_data) > 0) {
    combined <- all_data %>%
      group_by(date, country) %>%
      summarise(
        revenue = sum(revenue, na.rm = TRUE),
        downloads = sum(downloads, na.rm = TRUE),
        .groups = "drop"
      )
    return(combined)
  }
  
  return(all_data)
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
      # Standardize columns - check which columns exist
      ios_clean <- ios_data %>%
        dplyr::mutate(
          platform = "iOS",
          # Use conditional logic to handle missing columns
          revenue = if ("total_revenue" %in% names(.)) {
            total_revenue
          } else if (all(c("iphone_revenue", "ipad_revenue") %in% names(.))) {
            iphone_revenue + ipad_revenue
          } else if ("revenue" %in% names(.)) {
            revenue
          } else {
            0
          },
          downloads = if ("total_downloads" %in% names(.)) {
            total_downloads
          } else if (all(c("iphone_downloads", "ipad_downloads") %in% names(.))) {
            iphone_downloads + ipad_downloads
          } else if ("downloads" %in% names(.)) {
            downloads
          } else {
            0
          }
        ) %>%
        dplyr::select(date, country, revenue, downloads, platform)
      
      # Add app_id info
      ios_clean$app_id <- ios_app_id
      ios_clean$app_id_type <- "ios"
      
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
      
      # Add app_id info
      android_clean$app_id <- android_app_id
      android_clean$app_id_type <- "android"
      
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
    # Add platform info for combined data
    all_data$platform <- "unified"
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