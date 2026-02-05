#' Fetch Session Metrics Time Series Data
#'
#' Retrieves session metrics time series data (session count, session duration,
#' time spent) for apps from the Sensor Tower Usage Intelligence API.
#'
#' @param unified_app_id Character string or vector. Sensor Tower unified app ID(s)
#'   (24-character hex). Maximum 100 apps per request.
#' @param ios_app_id Character string or vector. iOS app ID(s) for non-unified queries.
#' @param android_app_id Character string or vector. Android package name(s) for
#'   non-unified queries.
#' @param start_date Date or character string. Start date in "YYYY-MM-DD" format.
#'   Data is available from 2021-01-01 onward.
#' @param end_date Date or character string. End date in "YYYY-MM-DD" format.
#' @param metrics Character vector. Metrics to retrieve. Options include:
#'   - "time_spent" (average seconds per user per day)
#'   - "total_time_spent" (total seconds across all users)
#'   - "session_duration" (average session length in seconds)
#'   - "session_count" (average sessions per user per day)
#'   - "total_session_count" (total sessions across all users)
#'   Default is c("session_count", "session_duration", "time_spent").
#' @param regions Character vector. Region/country codes (e.g., "US", "GB").
#'   Default is "US". Use NULL for all regions.
#' @param time_period Character string. Session metrics time period.
#'   Options: "day", "week". Default is "week".
#'   Returns averaged session metrics for each period within a month.
#' @param date_granularity Character string. Aggregate data by granularity.
#'   Options: "daily", "weekly", "monthly". Default is "monthly".
#'   Note: "daily" granularity may not be supported by the API for all apps;
#'   use "weekly" or "monthly" if you receive empty results with "daily".
#' @param os Character string. Filter by platform for unified apps.
#'   Options: "ios", "android", or NULL for both. Default is NULL.
#' @param breakdown Character string. Fields for data aggregation.
#'   Options: "unified_app_id", "app_id", "region". Default is "unified_app_id".
#' @param auth_token Character string. Your Sensor Tower API token.
#'   Defaults to environment variable SENSORTOWER_AUTH_TOKEN.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return A [tibble][tibble::tibble] with session metrics including:
#'   - **unified_app_id** or **app_id**: The app identifier
#'   - **date**: Date of the data point
#'   - **time_spent**: Average seconds spent per user per day
#'   - **total_time_spent**: Total seconds across all users
#'   - **session_duration**: Average session length in seconds
#'   - **session_count**: Average sessions per user per day
#'   - **total_session_count**: Total session count across all users
#'
#' @section Data Availability:
#' - Data is available from 2021-01-01 onward
#' - Session metrics require Usage Intelligence subscription
#' - Maximum 100 apps per request
#'
#' @examples
#' \dontrun{
#' # Get session metrics for a unified app
#' sessions <- st_session_metrics(
#'   unified_app_id = "5fbc3849d0b8414136857afc",
#'   start_date = "2024-01-01",
#'   end_date = "2024-12-01"
#' )
#'
#' # Get specific metrics with weekly granularity
#' sessions <- st_session_metrics(
#'   unified_app_id = "5fbc3849d0b8414136857afc",
#'   start_date = "2024-01-01",
#'   end_date = "2024-03-01",
#'   metrics = c("session_count", "session_duration"),
#'   date_granularity = "weekly"
#' )
#'
#' # Get session data for Android app directly
#' sessions <- st_session_metrics(
#'   android_app_id = "com.example.app",
#'   start_date = "2024-01-01",
#'   end_date = "2024-06-01"
#' )
#' }
#'
#' @seealso [st_retention()] for retention metrics,
#'   [st_demographics()] for user demographics,
#'   [st_batch_metrics()] for MAU/DAU/WAU metrics
#'
#' @export
#' @importFrom rlang %||% abort
#' @importFrom httr2 request req_url_path_append req_url_query req_headers
#' @importFrom httr2 req_perform resp_body_raw resp_status req_retry
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows mutate select
st_session_metrics <- function(unified_app_id = NULL,
                                ios_app_id = NULL,
                                android_app_id = NULL,
                                start_date,
                                end_date,
                                metrics = c("session_count", "session_duration", "time_spent"),
                                regions = "US",
                                time_period = "week",
                                date_granularity = "monthly",
                                os = NULL,
                                breakdown = "unified_app_id",
                                auth_token = NULL,
                                verbose = TRUE) {

  # Validate that at least one app ID is provided
  if (is.null(unified_app_id) && is.null(ios_app_id) && is.null(android_app_id)) {
    rlang::abort(c(
      "At least one app ID must be provided.",
      "i" = "Use unified_app_id, ios_app_id, or android_app_id parameter.",
      "i" = "Use st_app_info('app name') to find app IDs."
    ))
  }

  # Validate dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (start_date < as.Date("2021-01-01")) {
    rlang::warn("Session metrics data is only available from 2021-01-01 onward. Adjusting start_date.")
    start_date <- as.Date("2021-01-01")
  }

  # Validate metrics
valid_metrics <- c("time_spent", "total_time_spent", "session_duration",
                     "session_count", "total_session_count")
  metrics <- match.arg(metrics, valid_metrics, several.ok = TRUE)

  # Validate time_period
  time_period <- match.arg(time_period, c("day", "week"))

  # Validate date_granularity
  date_granularity <- match.arg(date_granularity, c("daily", "weekly", "monthly"))

  # Authentication
  auth_token_val <- resolve_auth_token(
    auth_token,
    error_message = "Authentication token not found. Set SENSORTOWER_AUTH_TOKEN environment variable."
  )

  # Determine which endpoint to use based on app ID type
  if (!is.null(unified_app_id)) {
    # Use the unified_apps endpoint
    result <- fetch_session_metrics_unified(
      app_ids = unified_app_id,
      start_date = start_date,
      end_date = end_date,
      metrics = metrics,
      regions = regions,
      time_period = time_period,
      date_granularity = date_granularity,
      os = os,
      breakdown = breakdown,
      auth_token = auth_token_val,
      verbose = verbose
    )
  } else {
    # Use the non-unified endpoint for platform-specific IDs
    app_ids <- c(ios_app_id, android_app_id)
    app_ids <- app_ids[!is.null(app_ids) & !is.na(app_ids)]

    result <- fetch_session_metrics_platform(
      app_ids = app_ids,
      start_date = start_date,
      end_date = end_date,
      metrics = metrics,
      regions = regions,
      time_period = time_period,
      date_granularity = date_granularity,
      breakdown = "app_id",
      auth_token = auth_token_val,
      verbose = verbose
    )
  }

  return(result)
}

#' Fetch session metrics for unified apps
#' @noRd
fetch_session_metrics_unified <- function(app_ids,
                                           start_date,
                                           end_date,
                                           metrics,
                                           regions,
                                           time_period,
                                           date_granularity,
                                           os,
                                           breakdown,
                                           auth_token,
                                           verbose = FALSE) {

  # Build query parameters
  query_params <- list(
    auth_token = auth_token,
    start_date = format(start_date, "%Y-%m-%d"),
    end_date = format(end_date, "%Y-%m-%d"),
    time_period = time_period,
    date_granularity = date_granularity,
    breakdown = breakdown
  )

  # Add app_ids as comma-separated
  query_params$app_ids <- paste(app_ids, collapse = ",")

  # Add metrics as comma-separated (API uses 'timeseries' param)
  query_params$timeseries <- paste(metrics, collapse = ",")

  # Add regions if specified
  if (!is.null(regions) && length(regions) > 0) {
    query_params$regions <- paste(regions, collapse = ",")
  }

  # Add OS filter if specified
  if (!is.null(os)) {
    query_params$os <- os
  }

  # Build request
  base_url <- st_api_base_url()

  if (verbose) {
    message("Fetching session metrics for ", length(app_ids), " unified app(s)...")
  }

  req <- build_request(
    base_url = base_url,
    path_segments = st_endpoint_segments("apps_timeseries_unified"),
    query_params = query_params
  ) %>%
    httr2::req_headers(
      "Accept" = "application/json"
    ) %>%
    httr2::req_timeout(60) %>%
    httr2::req_retry(max_tries = 3, backoff = function(i) 2^i)

  # Perform request
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    if (verbose) message("API request failed: ", e$message)
    return(NULL)
  })

  if (is.null(resp)) {
    return(tibble::tibble())
  }

  # Check status
  status <- httr2::resp_status(resp)
  if (status != 200) {
    body <- tryCatch(httr2::resp_body_string(resp), error = function(.) "")
    if (verbose) message(sprintf("API returned status %d: %s", status, body))
    return(tibble::tibble())
  }

  # Parse response
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(tibble::tibble())
  }

  body_text <- rawToChar(body_raw)
  result <- tryCatch({
    jsonlite::fromJSON(body_text, flatten = TRUE)
  }, error = function(e) {
    if (verbose) message("Failed to parse response: ", e$message)
    return(NULL)
  })

  if (is.null(result)) {
    return(tibble::tibble())
  }

  # Process the response
  # Expected format: { "unified_apps": [ { "unified_app_id": "...", "timeseries": [...] } ] }
  process_session_response(result, "unified", verbose)
}

#' Fetch session metrics for platform-specific apps
#' @noRd
fetch_session_metrics_platform <- function(app_ids,
                                            start_date,
                                            end_date,
                                            metrics,
                                            regions,
                                            time_period,
                                            date_granularity,
                                            breakdown,
                                            auth_token,
                                            verbose = FALSE) {

  # Build query parameters
  query_params <- list(
    auth_token = auth_token,
    start_date = format(start_date, "%Y-%m-%d"),
    end_date = format(end_date, "%Y-%m-%d"),
    time_period = time_period,
    date_granularity = date_granularity,
    breakdown = breakdown
  )

  # Add app_ids as comma-separated
  query_params$app_ids <- paste(app_ids, collapse = ",")

  # Add metrics as comma-separated
  query_params$timeseries <- paste(metrics, collapse = ",")

  # Add regions if specified
  if (!is.null(regions) && length(regions) > 0) {
    query_params$regions <- paste(regions, collapse = ",")
  }

  # Build request
  base_url <- st_api_base_url()

  if (verbose) {
    message("Fetching session metrics for ", length(app_ids), " platform-specific app(s)...")
  }

  req <- build_request(
    base_url = base_url,
    path_segments = st_endpoint_segments("apps_timeseries"),
    query_params = query_params
  ) %>%
    httr2::req_headers(
      "Accept" = "application/json"
    ) %>%
    httr2::req_timeout(60) %>%
    httr2::req_retry(max_tries = 3, backoff = function(i) 2^i)

  # Perform request
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    if (verbose) message("API request failed: ", e$message)
    return(NULL)
  })

  if (is.null(resp)) {
    return(tibble::tibble())
  }

  # Check status
  status <- httr2::resp_status(resp)
  if (status != 200) {
    body <- tryCatch(httr2::resp_body_string(resp), error = function(.) "")
    if (verbose) message(sprintf("API returned status %d: %s", status, body))
    return(tibble::tibble())
  }

  # Parse response
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(tibble::tibble())
  }

  body_text <- rawToChar(body_raw)
  result <- tryCatch({
    jsonlite::fromJSON(body_text, flatten = TRUE)
  }, error = function(e) {
    if (verbose) message("Failed to parse response: ", e$message)
    return(NULL)
  })

  if (is.null(result)) {
    return(tibble::tibble())
  }

  # Process the response
  process_session_response(result, "platform", verbose)
}

#' Process session metrics API response into a tibble
#' @noRd
process_session_response <- function(result, response_type, verbose = FALSE) {
  # Handle both unified_apps and apps response structures
  apps_data <- NULL

  if ("unified_apps" %in% names(result)) {
    apps_data <- result$unified_apps
  } else if ("apps" %in% names(result)) {
    apps_data <- result$apps
  } else if (is.list(result) && length(result) > 0) {
    # Try to use the result directly if it's a list of apps
    apps_data <- result
  }

  if (is.null(apps_data) || length(apps_data) == 0) {
    if (verbose) message("No session data found in response.")
    return(tibble::tibble())
  }

  # Process each app's timeseries data
  all_rows <- list()

  # Handle different data structures
  if (is.data.frame(apps_data)) {
    for (i in seq_len(nrow(apps_data))) {
      app_row <- apps_data[i, ]
      app_id <- app_row$unified_app_id %||% app_row$app_id %||% NA_character_

      timeseries <- app_row$timeseries
      if (is.list(timeseries) && length(timeseries) > 0) {
        ts_df <- timeseries[[1]]
        if (is.data.frame(ts_df) && nrow(ts_df) > 0) {
          ts_df$app_id <- app_id
          if ("unified_app_id" %in% names(app_row)) {
            ts_df$unified_app_id <- app_id
          }
          all_rows[[length(all_rows) + 1]] <- ts_df
        }
      }
    }
  } else if (is.list(apps_data)) {
    for (app_item in apps_data) {
      if (is.null(app_item)) next

      app_id <- app_item$unified_app_id %||% app_item$app_id %||% NA_character_
      timeseries <- app_item$timeseries

      if (!is.null(timeseries) && length(timeseries) > 0) {
        if (is.data.frame(timeseries)) {
          ts_df <- timeseries
        } else if (is.list(timeseries)) {
          ts_df <- tryCatch(
            dplyr::bind_rows(timeseries),
            error = function(e) tibble::tibble()
          )
        } else {
          next
        }

        if (nrow(ts_df) > 0) {
          ts_df$app_id <- app_id
          if (!is.null(app_item$unified_app_id)) {
            ts_df$unified_app_id <- app_id
          }
          all_rows[[length(all_rows) + 1]] <- ts_df
        }
      }
    }
  }

  if (length(all_rows) == 0) {
    if (verbose) message("No timeseries data found.")
    return(tibble::tibble())
  }

  # Combine all rows
  result_tbl <- dplyr::bind_rows(all_rows)

  # Convert date column
  if ("date" %in% names(result_tbl)) {
    result_tbl$date <- as.Date(result_tbl$date)
  }

  # Reorder columns to put identifiers first
  id_cols <- intersect(c("unified_app_id", "app_id", "date"), names(result_tbl))
  other_cols <- setdiff(names(result_tbl), id_cols)
  result_tbl <- result_tbl[, c(id_cols, other_cols)]

  if (verbose) {
    message(sprintf("Retrieved %d session data points.", nrow(result_tbl)))
  }

  return(tibble::as_tibble(result_tbl))
}
