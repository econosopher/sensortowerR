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
#' - os = "unified": Returns combined iOS + Android data (as separate rows)
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
#'   ios_app_id = "1195621598", # Homescapes iOS
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
    rlang::abort("'os' parameter is required and must be one of: 'ios', 'android', or 'unified'")
  }

  # Validate required parameters
  if (missing(countries) || is.null(countries) || length(countries) == 0) {
    rlang::abort("'countries' parameter is required. Specify country codes (e.g., 'US', 'GB', 'JP', or 'WW' for worldwide).")
  }

  if (missing(date_granularity) || is.null(date_granularity)) {
    rlang::abort("'date_granularity' parameter is required. Specify one of: 'daily', 'weekly', 'monthly', 'quarterly'.")
  }

  # Validate date_granularity value
  valid_granularities <- c("daily", "weekly", "monthly", "quarterly")
  if (!date_granularity %in% valid_granularities) {
    rlang::abort(paste0("Invalid date_granularity: '", date_granularity, "'. Must be one of: ", paste(valid_granularities, collapse = ", ")))
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
    rlang::abort("'start_date' must be earlier than or equal to 'end_date'")
  }

  # Check authentication
  if (is.null(auth_token) || auth_token == "") {
    rlang::abort("Authentication token required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }

  # Prepare IDs for unified fetcher
  final_ios_id <- NULL
  final_android_id <- NULL

  if (os == "unified") {
    # For unified, we need both IDs if possible
    if (!is.null(unified_app_id)) {
      # Look up platform IDs if we have a unified ID
      lookup_result <- tryCatch(
        {
          resolve_app_id(unified_app_id, auth_token = auth_token, verbose = verbose)
        },
        error = function(e) NULL
      )

      if (!is.null(lookup_result)) {
        final_ios_id <- lookup_result$ios_id
        final_android_id <- lookup_result$android_id
      }
    } else {
      # Use provided platform IDs
      final_ios_id <- ios_app_id
      final_android_id <- android_app_id
    }

    # Fallback to resolved IDs if still null (though resolve_ids_for_os handles most of this)
    if (is.null(final_ios_id) && !is.null(resolved_ids$ios_app_id)) final_ios_id <- resolved_ids$ios_app_id
    if (is.null(final_android_id) && !is.null(resolved_ids$android_app_id)) final_android_id <- resolved_ids$android_app_id
  } else if (os == "ios") {
    final_ios_id <- resolved_ids$ios_app_id
  } else if (os == "android") {
    final_android_id <- resolved_ids$android_app_id
  }

  # Fetch data using the unified engine
  result <- fetch_unified_data(
    ios_app_id = final_ios_id,
    android_app_id = final_android_id,
    start_date = start_date,
    end_date = end_date,
    countries = countries,
    date_granularity = date_granularity,
    auth_token = auth_token,
    verbose = verbose,
    combine_to_unified = FALSE
  )

  # Add app_id and app_id_type metadata for non-unified paths if not already present
  if (os != "unified" && !is.null(resolved_ids) && nrow(result) > 0) {
    # Ensure app_id/app_id_type cols exist and are correct
    # fetch_unified_data adds them, but let's be safe
    if (!"app_id" %in% names(result)) result$app_id <- resolved_ids[[1]]
    if (!"app_id_type" %in% names(result)) result$app_id_type <- app_id_type
  }

  return(result)
}
