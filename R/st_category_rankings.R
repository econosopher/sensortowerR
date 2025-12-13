#' Fetch App Store Category Rankings
#'
#' Retrieves the top ranking apps for a specific category and chart type from
#' the App Store or Google Play Store. This provides the official store rankings
#' as they appear in the actual app stores.
#'
#' @param os Character string. Required. Operating system: "ios", "android", or "unified".
#' @param category Character or numeric. Category ID to fetch rankings for.
#'   Use `st_categories()` to find valid category IDs. Required unless 
#'   `custom_fields_filter_id` is provided.
#' @param chart_type Character string. The chart type to retrieve. Options vary by OS:
#'   - iOS: "topfreeapplications", "toppaidapplications", "topgrossingapplications", etc.
#'   - Android: "topselling_free", "topselling_paid", "topgrossing", etc.
#'   Defaults to "topfreeapplications" for iOS, "topselling_free" for Android.
#' @param country Character string. Two-letter country code (e.g., "US", "GB").
#'   Defaults to "US".
#' @param date Date or character string in "YYYY-MM-DD" format. Date for rankings.
#'   Defaults to NULL (uses today's date).
#' @param limit Numeric. Number of results to return (1-400). Defaults to 100.
#' @param offset Numeric. Offset for pagination. Defaults to 0.
#' @param custom_fields_filter_id Optional. Character string. ID of a Sensor
#'   Tower custom field filter to apply. Use filter IDs from the web interface
#'   at app.sensortower.com. When provided, this filter will be applied to the
#'   results. The 'category' parameter becomes optional when using a custom filter.
#' @param custom_tags_mode Optional. Character string. Required if `os` is
#'   'unified' and `custom_fields_filter_id` is provided. Specifies how the
#'   custom filter applies to unified apps. Options: "include", "exclude",
#'   "include_unified_apps". The "include_unified_apps" option includes all
#'   platform versions when any version matches the filter.
#' @param auth_token Character string. Sensor Tower API authentication token.
#'   Defaults to environment variable SENSORTOWER_AUTH_TOKEN.
#'
#' @return A [tibble][tibble::tibble] containing ranking data with columns:
#'   - `rank`: The app's position in the chart
#'   - `app_id`: The app's store ID
#'   - `category`: The category ID
#'   - `country`: The country code
#'   - `date`: The ranking date
#'   - `chart_type`: The chart type
#'   - `os`: The operating system
#'   
#' @note The API returns only app IDs, not names. To get app names and other
#'   metadata, use the app IDs with `st_app_details()`.
#'
#' @section API Endpoint Used:
#'   - `GET /v1/{os}/ranking`
#'
#' @examples
#' \dontrun{
#' # Get top free games in the US
#' top_games <- st_category_rankings(
#'   os = "ios",
#'   category = 6014,  # Games category
#'   chart_type = "topfreeapplications",
#'   country = "US",
#'   limit = 50
#' )
#'
#' # Get top grossing apps in UK for a specific date
#' top_grossing <- st_category_rankings(
#'   os = "android",
#'   category = "game",
#'   chart_type = "topgrossing",
#'   country = "GB",
#'   date = "2024-01-15",
#'   limit = 100
#' )
#'
#' # Use custom filter instead of category
#' filtered_rankings <- st_category_rankings(
#'   os = "ios",
#'   custom_fields_filter_id = "60746340241bc16eb8a65d76",
#'   chart_type = "topgrossingapplications",
#'   country = "US",
#'   limit = 50
#' )
#'
#' # With unified OS and custom filter
#' unified_rankings <- st_category_rankings(
#'   os = "unified",
#'   custom_fields_filter_id = "60746340241bc16eb8a65d76",
#'   custom_tags_mode = "include_unified_apps",
#'   chart_type = "topfreeapplications"
#' )
#' }
#'
#' @importFrom rlang %||% abort
#' @importFrom httr2 resp_body_raw
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr rename
#' @export
st_category_rankings <- function(os,
                                category = NULL,
                                chart_type = NULL,
                                country = "US",
                                date = NULL,
                                limit = 100,
                                offset = 0,
                                custom_fields_filter_id = NULL,
                                custom_tags_mode = NULL,
                                auth_token = NULL) {
  
  # Validate OS parameter
  if (missing(os) || is.null(os) || !os %in% c("ios", "android", "unified")) {
    rlang::abort("'os' parameter is required and must be one of: 'ios', 'android', or 'unified'")
  }
  
  # Load custom filter utilities if available
  if (exists("validate_custom_filter_params", mode = "function")) {
    validate_custom_filter_params(
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode,
      os = os,
      require_category = TRUE,
      category = category
    )
  } else {
    # Fallback validation if utilities not loaded
    if (is.null(category) && is.null(custom_fields_filter_id)) {
      rlang::abort("Either 'category' or 'custom_fields_filter_id' parameter is required.")
    }
  }
  
  # Set default chart type based on OS
  if (is.null(chart_type)) {
    chart_type <- switch(os,
      ios = "topfreeapplications",
      android = "topselling_free",
      unified = "topfreeapplications"
    )
  }
  
  # Validate limit
  if (limit < 1 || limit > 400) {
    rlang::abort("Limit must be between 1 and 400.")
  }
  
  # Authentication
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      c("Authentication token not found.",
        "Set SENSORTOWER_AUTH_TOKEN environment variable or pass via auth_token argument.")
    )
  }
  
  # Build query parameters
  query_params <- list(
    auth_token = auth_token_val,
    chart_type = chart_type,
    country = country,
    limit = limit,
    offset = offset
  )
  
  # Only add category if provided
  if (!is.null(category)) {
    query_params$category <- as.character(category)
  }
  
  # Add date - API requires it even though documentation suggests optional
  if (!is.null(date)) {
    if (inherits(date, "Date")) {
      date <- format(date, "%Y-%m-%d")
    }
    query_params$date = date
  } else {
    # Default to today's date
    query_params$date = format(Sys.Date(), "%Y-%m-%d")
  }
  
  # Add custom filter parameters if available
  if (exists("add_custom_filter_params", mode = "function")) {
    query_params <- add_custom_filter_params(
      query_params,
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode,
      os = os
    )
  } else if (!is.null(custom_fields_filter_id)) {
    # Fallback if utilities not loaded
    query_params$custom_fields_filter_id <- custom_fields_filter_id
    if (!is.null(custom_tags_mode)) {
      query_params$custom_tags_mode <- custom_tags_mode
    }
  }
  
  # Build and perform request
  path <- c("v1", os, "ranking")
  req <- build_request("https://api.sensortower.com", path, query_params)
  resp <- perform_request(req)
  
  # Process response
  result <- process_ranking_response(resp, os, category, country, chart_type, date, limit)
  
  return(result)
}

#' Process Category Ranking API Response
#'
#' Internal function to process and enrich category ranking API responses.
#'
#' @param resp Response object from httr2
#' @param os Operating system
#' @param category Category ID used in request
#' @param country Country code used in request
#' @param chart_type Chart type used in request
#' @param date Date used in request (may be NULL)
#' @param limit Number of results requested
#'
#' @return A processed tibble with ranking data
#' @keywords internal
process_ranking_response <- function(resp, os, category, country, chart_type, date, limit) {
  
  # Get raw response
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(tibble::tibble())
  }
  
  body_text <- rawToChar(body_raw)
  result <- jsonlite::fromJSON(body_text, flatten = TRUE)
  
  if (length(result) == 0) {
    return(tibble::tibble())
  }
  
  # Convert to tibble
  result_tbl <- tibble::as_tibble(result)
  
  if (nrow(result_tbl) == 0) {
    return(result_tbl)
  }
  
  # Add ranking position (API returns ordered list)
  result_tbl$rank <- seq_len(nrow(result_tbl))
  
  # Reorder columns for better readability
  col_order <- c("rank")
  
  # The API returns a simple array of app IDs in the 'ranking' field
  # We need to restructure this into a more useful format
  if ("ranking" %in% names(result_tbl)) {
    # Extract app IDs from the ranking column
    app_ids <- result_tbl$ranking
    
    # Create a new tibble with proper structure
    result_tbl <- tibble::tibble(
      rank = result_tbl$rank,
      app_id = as.character(app_ids)
    )
  }
  
  # Add context columns
  result_tbl$category <- category
  result_tbl$country <- country
  result_tbl$chart_type <- chart_type
  result_tbl$os <- os
  
  # Add date if available
  if (!is.null(date)) {
    result_tbl$date <- as.Date(date)
  } else {
    result_tbl$date <- Sys.Date()  # Use today's date as approximation
  }
  
  # Define preferred column order
  preferred_cols <- c("rank", "app_id", "app_name", "publisher_name", "category", 
                     "country", "date", "chart_type", "os")
  
  # Reorder columns, keeping any additional fields at the end
  existing_cols <- intersect(preferred_cols, names(result_tbl))
  other_cols <- setdiff(names(result_tbl), preferred_cols)
  
  result_tbl <- result_tbl[, c(existing_cols, other_cols)]
  
  # Apply limit since API seems to ignore it
  if (nrow(result_tbl) > limit) {
    result_tbl <- result_tbl[1:limit, ]
  }
  
  # Add platform information
  result_tbl$platform <- os
  
  return(result_tbl)
}

