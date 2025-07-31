#' Fetch Sales Report Estimates
#'
#' Retrieves download and revenue estimates of apps by country and date.
#' Note: All revenues are returned in cents and need to be divided by 100 for dollar amounts.
#'
#' @param ios_app_id Character string. iOS app ID (numeric, e.g., "1234567890").
#' @param android_app_id Character string. Android package name (e.g., "com.example.app").
#' @param unified_app_id Character string. Sensor Tower unified app ID (24-character hex).
#' @param publisher_ids Character vector. Publisher IDs to query. Some Android publisher IDs contain commas.
#' @param custom_fields_filter_id Optional. Character string. ID of a Sensor
#'   Tower custom field filter to apply. Use filter IDs from the web interface
#'   at app.sensortower.com. When provided, this filter will be used instead of
#'   app_ids or publisher_ids.
#' @param custom_tags_mode Optional. Character string. Required if `os` is
#'   'unified' and `custom_fields_filter_id` is provided. Specifies how the
#'   custom filter applies to unified apps. Options: "include", "exclude",
#'   "include_unified_apps". The "include_unified_apps" option includes all
#'   platform versions when any version matches the filter.
#' @param os Character string. Required. Operating system: "ios", "android", or "unified".
#' @param countries Character vector. Country codes (e.g., c("US", "GB", "JP"), or "WW" for worldwide). Required.
#' @param start_date Date or character string. Start date in "YYYY-MM-DD" format. Required.
#' @param end_date Date or character string. End date in "YYYY-MM-DD" format. Required.
#' @param date_granularity Character string. One of "daily", "weekly", "monthly", "quarterly". Required.
#' @param limit Numeric. Number of results to return when using custom_fields_filter_id.
#'   Ignored when using specific app ID parameters or publisher_ids. Defaults to 100.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param auto_segment Logical. If TRUE, automatically segments date ranges to avoid timeouts.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return A tibble with download and revenue estimates.
#'
#' @details
#' **App ID Parameters**: Provide one of the following:
#' - `ios_app_id`: Specifically for iOS app IDs (numeric)
#' - `android_app_id`: Specifically for Android package names
#' - `unified_app_id`: Specifically for Sensor Tower unified IDs
#'
#' The function will automatically resolve IDs if needed. For example, if you provide
#' a `unified_app_id` but set `os="ios"`, it will look up the iOS app ID.
#'
#' The API has timeout limitations based on date granularity:
#' - daily: limit to 1 week segments
#' - weekly: limit to 3 month segments  
#' - monthly: limit to 1 year segments
#' - quarterly: limit to 2 year segments
#' 
#' When auto_segment = TRUE, the function automatically breaks up the date range
#' into appropriate segments and combines the results.
#' 
#'
#' @examples
#' \dontrun{
#' # Get daily sales for a single app using specific parameter
#' sales <- st_sales_report(
#'   os = "ios",
#'   ios_app_id = "553834731",  # Candy Crush iOS
#'   countries = c("US", "GB"),
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-07",
#'   date_granularity = "daily"
#' )
#' 
#' # Get Android data using specific parameter
#' android_sales <- st_sales_report(
#'   os = "android",
#'   android_app_id = "com.king.candycrushsaga",
#'   countries = "US",
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-07",
#'   date_granularity = "daily"
#' )
#' 
#' # Get iOS data from unified ID (automatic lookup)
#' unified_sales <- st_sales_report(
#'   os = "ios",
#'   unified_app_id = "5ba4585f539ce75b97db6bcb",
#'   countries = "US",
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-07",
#'   date_granularity = "daily"
#' )
#' 
#' }
#'
#' @export
st_sales_report <- function(os,
                           countries,
                           start_date,
                           end_date,
                           date_granularity,
                           ios_app_id = NULL,
                           android_app_id = NULL,
                           unified_app_id = NULL,
                            publisher_ids = NULL,
                           custom_fields_filter_id = NULL,
                           custom_tags_mode = NULL,
                           limit = 100,
                           auth_token = NULL,
                           auto_segment = TRUE,
                           verbose = TRUE) {
  
  # Validate OS parameter
  if (missing(os) || is.null(os) || !os %in% c("ios", "android", "unified")) {
    stop("'os' parameter is required and must be one of: 'ios', 'android', or 'unified'")
  }
  
  
  # Validate required parameters
  if (missing(countries) || is.null(countries) || length(countries) == 0) {
    stop("'countries' parameter is required. Specify country codes (e.g., 'US', 'GB', 'JP', or 'WW' for worldwide).")
  }
  
  if (missing(start_date) || is.null(start_date)) {
    stop("'start_date' parameter is required. Specify in YYYY-MM-DD format.")
  }
  
  if (missing(end_date) || is.null(end_date)) {
    stop("'end_date' parameter is required. Specify in YYYY-MM-DD format.")
  }
  
  if (missing(date_granularity) || is.null(date_granularity)) {
    stop("'date_granularity' parameter is required. Specify one of: 'daily', 'weekly', 'monthly', 'quarterly'.")
  }
  
  # Load custom filter utilities if available
  if (exists("validate_custom_filter_params", mode = "function")) {
    validate_custom_filter_params(
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode,
      os = os,
      require_category = FALSE
    )
  }
  
  # Input validation
  if (is.null(ios_app_id) && is.null(android_app_id) && is.null(unified_app_id) && is.null(publisher_ids) && is.null(custom_fields_filter_id)) {
    stop("At least one of ios_app_id, android_app_id, unified_app_id, publisher_ids, or custom_fields_filter_id is required")
  }
  date_granularity <- match.arg(date_granularity, c("daily", "weekly", "monthly", "quarterly"))
  
  # Convert dates to Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Authentication
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    stop("Authentication token not found. Please set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  # For unified OS, we don't support st_sales_report
  if (os == "unified") {
    stop(paste0(
      "st_sales_report does not support os='unified'.\n",
      "Please use platform-specific calls with os='ios' or os='android'.\n",
      "For unified data, consider using st_metrics() instead."
    ))
  }
  
  # Use the ID resolution system for consistency with st_metrics
  resolved_ids <- resolve_ids_for_os(
    unified_app_id = unified_app_id,
    ios_app_id = ios_app_id,
    android_app_id = android_app_id,
    os = os,
    auth_token = auth_token_val,
    verbose = verbose
  )
  
  # Extract the resolved app ID for the API call
  # The API expects app_ids parameter internally
  app_ids <- NULL
  if (os == "ios" && !is.null(resolved_ids$ios_app_id)) {
    app_ids <- resolved_ids$ios_app_id
  } else if (os == "android" && !is.null(resolved_ids$android_app_id)) {
    app_ids <- resolved_ids$android_app_id
  }
  
  # Determine date segments if auto_segment is TRUE
  if (auto_segment) {
    segments <- get_date_segments(start_date, end_date, date_granularity)
    if (verbose && nrow(segments) > 1) {
      message(sprintf("Breaking date range into %d segments to avoid timeouts", nrow(segments)))
    }
  } else {
    segments <- tibble(start = start_date, end = end_date)
  }
  
  # Collect results from all segments
  all_results <- list()
  
  for (i in seq_len(nrow(segments))) {
    segment_start <- segments$start[i]
    segment_end <- segments$end[i]
    
    if (verbose && nrow(segments) > 1) {
      message(sprintf("Fetching segment %d/%d: %s to %s", 
                     i, nrow(segments), segment_start, segment_end))
    }
    
    # Build query parameters
    query_params <- list(
      auth_token = auth_token_val,
      start_date = format(segment_start, "%Y-%m-%d"),
      end_date = format(segment_end, "%Y-%m-%d"),
      date_granularity = date_granularity,
      countries = paste(countries, collapse = ",")
    )
    
    # Add app IDs, publisher IDs, or custom filter
    if (!is.null(custom_fields_filter_id)) {
      query_params$limit <- limit
      # Add custom filter parameters if available
      if (exists("add_custom_filter_params", mode = "function")) {
        query_params <- add_custom_filter_params(
          query_params,
          custom_fields_filter_id = custom_fields_filter_id,
          custom_tags_mode = custom_tags_mode,
          os = os
        )
      } else {
        query_params$custom_fields_filter_id <- custom_fields_filter_id
        if (!is.null(custom_tags_mode)) {
          query_params$custom_tags_mode <- custom_tags_mode
        }
      }
    } else {
      if (!is.null(app_ids)) {
        query_params$app_ids <- paste(app_ids, collapse = ",")
      }
      
      if (!is.null(publisher_ids)) {
        # Handle publisher IDs with commas using array format
        if (any(grepl(",", publisher_ids))) {
          query_params$`publisher_ids[]` <- publisher_ids
        } else {
          query_params$publisher_ids <- paste(publisher_ids, collapse = ",")
        }
      }
    }
    
    # Build and perform request
    path <- c("v1", os, "sales_report_estimates")
    req <- build_request("https://api.sensortower.com", path, query_params)
    resp <- perform_request(req)
    
    # Process response
    result <- process_sales_response(resp, os)
    
    if (!is.null(result) && nrow(result) > 0) {
      all_results[[i]] <- result
    }
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    final_result <- bind_rows(all_results)
    
    if (verbose) {
      message(sprintf("Retrieved %d records", nrow(final_result)))
    }
    
    # Add platform information
    final_result$platform <- os
    
    return(final_result)
  } else {
    return(tibble())
  }
}

#' Get date segments based on granularity
#' @noRd
get_date_segments <- function(start_date, end_date, granularity) {
  # Define segment lengths based on API recommendations
  segment_days <- switch(granularity,
    daily = 7,      # 1 week
    weekly = 90,    # ~3 months
    monthly = 365,  # 1 year
    quarterly = 730 # 2 years
  )
  
  # Create segments
  segments <- tibble()
  current_start <- start_date
  
  while (current_start <= end_date) {
    current_end <- min(current_start + segment_days - 1, end_date)
    segments <- bind_rows(segments, 
                         tibble(start = current_start, end = current_end))
    current_start <- current_end + 1
  }
  
  return(segments)
}

#' Process sales report response
#' @noRd
process_sales_response <- function(resp, os) {
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(NULL)
  }
  
  body_text <- rawToChar(body_raw)
  result <- jsonlite::fromJSON(body_text, flatten = TRUE)
  
  if (length(result) == 0 || nrow(result) == 0) {
    return(NULL)
  }
  
  # Convert to tibble
  result_tbl <- as_tibble(result)
  
  # Process based on OS
  if (os == "ios") {
    # Rename columns for clarity
    result_tbl <- result_tbl %>%
      rename_with(~ case_when(
        . == "aid" ~ "app_id",
        . == "cc" ~ "country",
        . == "d" ~ "date",
        . == "iu" ~ "iphone_downloads",
        . == "ir" ~ "iphone_revenue_cents",
        . == "au" ~ "ipad_downloads",
        . == "ar" ~ "ipad_revenue_cents",
        TRUE ~ .
      ))
    
    # Add calculated fields
    result_tbl <- result_tbl %>%
      mutate(
        date = as.Date(date),
        iphone_revenue = iphone_revenue_cents / 100,
        ipad_revenue = ipad_revenue_cents / 100,
        total_downloads = iphone_downloads + ipad_downloads,
        total_revenue = iphone_revenue + ipad_revenue,
        .after = date
      )
  } else {
    # Android
    result_tbl <- result_tbl %>%
      rename_with(~ case_when(
        . == "aid" ~ "app_id",
        . == "cc" ~ "country",
        . == "d" ~ "date",
        . == "u" ~ "downloads",
        . == "r" ~ "revenue_cents",
        TRUE ~ .
      ))
    
    # Add calculated fields
    result_tbl <- result_tbl %>%
      mutate(
        date = as.Date(date),
        revenue = revenue_cents / 100,
        .after = date
      )
  }
  
  return(result_tbl)
}

#' Helper function to look up category names
#'
#' @param category_ids Character vector of category IDs
#' @param platform Character string. "ios" or "android"
#' @return Character vector of category names
#' @importFrom purrr map_chr
#' @export
lookup_category_names <- function(category_ids, platform = "ios") {
  # Load internal data
  st_category_data <- NULL
  data("st_category_data", envir = environment())
  
  # Filter for platform and match IDs
  categories <- st_category_data %>%
    filter(platform == !!platform) %>%
    filter(category_id %in% category_ids)
  
  # Return matched names in same order as input
  category_ids %>%
    map_chr(~ {
      matched <- categories %>%
        filter(category_id == .x) %>%
        pull(category_name)
      
      if (length(matched) > 0) {
        matched[1]
      } else {
        as.character(.x)  # Return ID if no match found
      }
    })
}