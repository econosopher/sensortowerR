#' Fetch Game Market Summary Data
#'
#' Retrieves aggregate download and revenue estimates by game categories,
#' countries, and date ranges from Sensor Tower's `games_breakdown` endpoint.
#' Use this for market/category denominator series; do not approximate market
#' totals by batching top charts, rankings, or large app rosters.
#'
#' @param categories Character string or numeric vector. Game category IDs to 
#'   analyze. Defaults to 7001 (a popular game category). Use `st_categories()` 
#'   to find valid category IDs.
#' @param countries Character vector or comma-separated string. Country codes 
#'   (e.g., `"US"`, `c("US", "GB")`, `"WW"` for worldwide) to analyze. Required.
#' @param os Character string. Operating System. Must be one of "ios", "android", 
#'   or "unified". Required. Note: The underlying API only supports "ios" and "android";
#'   when `os = "unified"` this function automatically fetches both platforms and
#'   combines them into a single table with total columns.
#' @param date_granularity Character string. Time granularity for aggregation. 
#'   Must be one of "daily", "weekly", "monthly", or "quarterly". Required.
#' @param start_date Character string or Date object. Start date for the query 
#'   in "YYYY-MM-DD" format. Required.
#' @param end_date Character string or Date object. End date for the query 
#'   in "YYYY-MM-DD" format, inclusive. Required.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param base_url Optional. Character string. The base URL for the API.
#' @param enrich_response Optional. Logical. If `TRUE` (default), enriches
#'   the response with readable column names and processes the data.
#'
#' @return A [tibble][tibble::tibble] with aggregate game market summary data including:
#'   - **Category information**: Game category details
#'   - **Geographic data**: Country-level breakdowns
#'   - **Downloads**: iOS (iPhone + iPad combined) and Android download estimates
#'   - **Revenue**: iOS (iPhone + iPad combined) and Android revenue estimates,
#'     expressed in dollars when `enrich_response = TRUE`
#'   - **Totals (unified only)**: `Total Downloads`, `Total Revenue`
#'   - **Time series**: Data broken down by specified granularity
#'   
#'   **Automatic Data Combination**: For iOS and unified platforms, iPhone and iPad
#'   data are automatically combined into single "iOS Downloads" and "iOS Revenue" 
#'   columns for simplified analysis.
#'
#' @section API Endpoint Used:
#'   - **Game Summary**: `GET /v1/\{os\}/games_breakdown`
#'     (API only supports `os = "ios"` or `os = "android"`; unified is synthesized)
#'
#' @section Revenue Units:
#'   The raw `games_breakdown` endpoint returns revenue in cents. With
#'   `enrich_response = TRUE`, this function converts revenue fields to dollars
#'   before combining devices or platforms. With `enrich_response = FALSE`, raw
#'   endpoint fields are returned unchanged.
#'
#' @section Field Mappings and Processing:
#'   The API returns abbreviated field names which are automatically mapped to 
#'   descriptive names and processed:
#'   - **iOS**: `iu` + `au` = iOS Downloads (combined), `ir` + `ar` = iOS Revenue (combined)
#'   - **Android**: `u` = Android Downloads, `r` = Android Revenue
#'   - **Common**: `ca` = Category, `cc` = Country Code, `d` = Date
#'   
#'   iPhone and iPad data are automatically combined for simplified analysis.
#'   For `os = "unified"`, iOS and Android aggregate rows are summed by date and
#'   country across the requested category basket.
#'
#' @examples
#' \dontrun{
#' # Specific categories and countries
#' rpg_summary <- st_game_summary(
#'   categories = c(7001, 7002),
#'   countries = c("US", "GB", "DE"),
#'   os = "ios",
#'   date_granularity = "weekly",
#'   start_date = "2024-01-01",
#'   end_date = "2024-03-31"
#' )
#'
#' # Monthly summary for iOS games in the US
#' ios_monthly <- st_game_summary(
#'   os = "ios",
#'   countries = "US", 
#'   date_granularity = "monthly",
#'   start_date = "2024-01-01",
#'   end_date = "2024-06-30"
#' )
#' }
#'
#' @seealso [st_categories()], [st_rankings()], [st_metrics()]
#' @export
st_game_summary <- function(categories = 7001,
                            countries,
                            os,
                            date_granularity,
                            start_date,
                            end_date,
                            auth_token = NULL,
                            base_url = "https://api.sensortower.com",
                            enrich_response = TRUE) {
  
  # Validate required parameters
  if (missing(countries) || is.null(countries) || length(countries) == 0) {
    rlang::abort("'countries' parameter is required. Specify country codes (e.g., 'US', 'GB', 'JP', or 'WW' for worldwide).")
  }
  
  if (missing(os) || is.null(os)) {
    rlang::abort("'os' parameter is required. Specify one of: 'ios', 'android', 'unified'.")
  }
  
  if (missing(date_granularity) || is.null(date_granularity)) {
    rlang::abort("'date_granularity' parameter is required. Specify one of: 'daily', 'weekly', 'monthly', 'quarterly'.")
  }
  
  if (missing(start_date) || is.null(start_date)) {
    rlang::abort("'start_date' parameter is required. Specify in YYYY-MM-DD format.")
  }
  
  if (missing(end_date) || is.null(end_date)) {
    rlang::abort("'end_date' parameter is required. Specify in YYYY-MM-DD format.")
  }
  
  # Validate inputs
  if (!date_granularity %in% c("daily", "weekly", "monthly", "quarterly")) {
    rlang::abort("date_granularity must be one of: daily, weekly, monthly, quarterly")
  }
  
  if (!os %in% c("ios", "android", "unified")) {
    rlang::abort("os must be one of: ios, android, unified")
  }
  
  # --- Authentication ---
  auth_token_val <- resolve_auth_token(
    auth_token,
    error_message = "Authentication token not found. Please set it as an environment variable."
  )
  
  # Convert dates to proper format
  start_date <- as.character(as.Date(start_date))
  end_date <- as.character(as.Date(end_date))
  
  # Convert categories to comma-separated string if vector
  if (is.numeric(categories) || length(categories) > 1) {
    categories <- paste(categories, collapse = ",")
  }
  
  # Convert countries to comma-separated string if vector
  if (length(countries) > 1) {
    countries <- paste(countries, collapse = ",")
  }
  
  # If unified requested, fetch iOS + Android and combine
  if (os == "unified") {
    ios_res <- tryCatch(
      st_game_summary(
        categories = categories,
        countries = countries,
        os = "ios",
        date_granularity = date_granularity,
        start_date = start_date,
        end_date = end_date,
        auth_token = auth_token_val,
        base_url = base_url,
        enrich_response = enrich_response
      ),
      error = function(e) tibble::tibble()
    )
    android_res <- tryCatch(
      st_game_summary(
        categories = categories,
        countries = countries,
        os = "android",
        date_granularity = date_granularity,
        start_date = start_date,
        end_date = end_date,
        auth_token = auth_token_val,
        base_url = base_url,
        enrich_response = enrich_response
      ),
      error = function(e) tibble::tibble()
    )

    if (nrow(ios_res) == 0 && nrow(android_res) == 0) {
      return(tibble::tibble())
    }

    if (!enrich_response) {
      return(dplyr::bind_rows(
        dplyr::mutate(ios_res, os = "ios"),
        dplyr::mutate(android_res, os = "android")
      ))
    }

    unified_keys <- game_summary_unified_keys(ios_res, android_res)
    ios_res <- summarize_game_summary_platform(
      ios_res,
      metric_cols = c("iOS Downloads", "iOS Revenue"),
      keys = unified_keys
    )
    android_res <- summarize_game_summary_platform(
      android_res,
      metric_cols = c("Android Downloads", "Android Revenue"),
      keys = unified_keys
    )

    joined <- dplyr::full_join(ios_res, android_res, by = unified_keys)

    # Add total columns when platform-specific metrics exist
    if ("iOS Downloads" %in% names(joined) || "Android Downloads" %in% names(joined)) {
      joined$`Total Downloads` <- row_sum_metric_columns(
        joined,
        intersect(c("iOS Downloads", "Android Downloads"), names(joined))
      )
    }
    if ("iOS Revenue" %in% names(joined) || "Android Revenue" %in% names(joined)) {
      joined$`Total Revenue` <- row_sum_metric_columns(
        joined,
        intersect(c("iOS Revenue", "Android Revenue"), names(joined))
      )
    }

    return(joined)
  }

  # Build API request (platform-specific)
  path_segments <- st_endpoint_segments("games_breakdown", os = os)
  
  query_params <- list(
    auth_token = auth_token_val,
    categories = categories,
    date_granularity = date_granularity,
    start_date = start_date,
    end_date = end_date
  )
  
  # Add countries parameter (API uses "WW" for worldwide by default)
  if (!is.null(countries) && countries != "WW") {
    query_params$countries <- countries
  }
  
  # Build and perform request
  req <- build_request(base_url, path_segments, query_params)
  resp <- perform_request(req)
  
  # Process response
  if (enrich_response) {
    result <- process_game_summary_response(resp, os)
  } else {
    result <- process_response(resp, FALSE)
  }
  
  return(result)
}

#' Process Game Summary API Response
#'
#' Internal function to process and enrich game summary API responses.
#'
#' @param resp List. Raw API response from game summary endpoint.
#' @param os Character string. Operating system to determine field mappings.
#'
#' @return A processed tibble with descriptive column names.
#' @keywords internal
process_game_summary_response <- function(resp, os) {
  
  # First get the raw response like process_response does
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
  
  # Apply field name mappings using the games_breakdown_key data
  result_tbl <- map_game_summary_fields(result_tbl, os)
  
  # Convert date fields to proper Date format
  if ("Date" %in% names(result_tbl)) {
    result_tbl$Date <- as.Date(substr(result_tbl$Date, 1, 10))
  }
  
  download_cols <- intersect(
    c("iPhone Downloads", "iPad Downloads", "Android Downloads"),
    names(result_tbl)
  )
  revenue_cols <- intersect(
    c("iPhone Revenue", "iPad Revenue", "Android Revenue"),
    names(result_tbl)
  )

  for (col in download_cols) {
    result_tbl[[col]] <- as.numeric(result_tbl[[col]])
  }

  for (col in revenue_cols) {
    result_tbl[[col]] <- as.numeric(result_tbl[[col]]) / 100
  }
  
  # Automatically combine iPad and iPhone data into unified iOS totals
  if (os %in% c("ios", "unified")) {
    # Check if we have both iPhone and iPad columns
    has_iphone_downloads <- "iPhone Downloads" %in% names(result_tbl)
    has_ipad_downloads <- "iPad Downloads" %in% names(result_tbl)
    has_iphone_revenue <- "iPhone Revenue" %in% names(result_tbl)
    has_ipad_revenue <- "iPad Revenue" %in% names(result_tbl)
    
    if (has_iphone_downloads && has_ipad_downloads) {
      result_tbl$`iOS Downloads` <- row_sum_metric_columns(
        result_tbl,
        c("iPhone Downloads", "iPad Downloads")
      )
      # Remove individual device columns
      result_tbl$`iPhone Downloads` <- NULL
      result_tbl$`iPad Downloads` <- NULL
    }
    
    if (has_iphone_revenue && has_ipad_revenue) {
      result_tbl$`iOS Revenue` <- row_sum_metric_columns(
        result_tbl,
        c("iPhone Revenue", "iPad Revenue")
      )
      # Remove individual device columns
      result_tbl$`iPhone Revenue` <- NULL
      result_tbl$`iPad Revenue` <- NULL
    }
  }
  
  return(result_tbl)
}

#' Map Game Summary Field Names
#'
#' Internal function to map abbreviated API field names to descriptive names.
#'
#' @param data Tibble. Data with abbreviated field names.
#' @param os Character string. Operating system to determine mappings.
#'
#' @return Tibble with descriptive field names.
#' @keywords internal
map_game_summary_fields <- function(data, os) {
  
  # Get the appropriate field mappings from internal data
  if (os == "unified") {
    # For unified, we need to handle both iOS and Android fields
    ios_mappings <- games_breakdown_key$ios
    android_mappings <- games_breakdown_key$android
    
    # Combine mappings, with iOS taking precedence for common fields
    field_mappings <- c(ios_mappings, android_mappings[!names(android_mappings) %in% names(ios_mappings)])
  } else {
    field_mappings <- games_breakdown_key[[os]]
  }

  field_mappings <- c(
    list(ca = "Category"),
    field_mappings
  )
  
  if (is.null(field_mappings)) {
    warning("Unknown OS: ", os, ". Field names will not be mapped.")
    return(data)
  }
  
  # Apply mappings
  current_names <- names(data)
  new_names <- current_names
  
  for (i in seq_along(current_names)) {
    old_name <- current_names[i]
    if (old_name %in% names(field_mappings)) {
      new_names[i] <- field_mappings[[old_name]]
    }
  }
  
  names(data) <- new_names
  return(data)
} 

game_summary_unified_keys <- function(ios_data, android_data) {
  candidate_keys <- c("Country Code", "Date")
  intersect(candidate_keys, intersect(names(ios_data), names(android_data)))
}

summarize_game_summary_platform <- function(data, metric_cols, keys) {
  if (nrow(data) == 0) {
    return(data)
  }

  metric_cols <- intersect(metric_cols, names(data))
  keep_cols <- c(keys, metric_cols)
  data <- data[, keep_cols, drop = FALSE]

  if (length(metric_cols) == 0) {
    return(data)
  }

  if (length(keys) == 0) {
    return(tibble::as_tibble(stats::setNames(
      lapply(metric_cols, function(col) sum_metric_column(data[[col]])),
      metric_cols
    )))
  }

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(metric_cols),
        sum_metric_column
      ),
      .groups = "drop"
    )
}

sum_metric_column <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}

row_sum_metric_columns <- function(data, cols) {
  cols <- intersect(cols, names(data))
  if (length(cols) == 0) {
    return(rep(NA_real_, nrow(data)))
  }

  values <- as.data.frame(lapply(data[, cols, drop = FALSE], as.numeric))
  out <- rowSums(values, na.rm = TRUE)
  out[rowSums(!is.na(values)) == 0] <- NA_real_
  out
}
